import strscans, strutils, streams, os, sequtils, algorithm, parseutils, strformat
import cligen

## The node information by default should be contained in the
## mesh.dat file
const defaultNodeFile = "mesh.dat"
const defaultPotentialFile = "potential.csv"

type
  Node = object
    id: int
    pos: tuple[x, y, z: float]
    volt: float # will be filled with the voltages at each node
                # after parsing the potential file

  Element = object
    id: int
    matId: int # ID of the materiel this element uses
    nodeIds: seq[int] # only need ids
    # an element has a bunch more properties, but we don't need those as far as
    # I'm concerned

  ElementGroup = object
    num: int
    elements: seq[Element]

  MeshFile = object
    nodes: seq[Node]
    groups: seq[ElementGroup]

func `$`(n: Node): string =
  ## brings a node into the string representation required for
  ## NLIST.lis
  ##
  ## The input from Discovery Aim uses `m` for the coordinates, but
  ## garfield uses `cm`, so convert.
  result = newStringOfCap(80)
  result.add align($n.id, 20)
  result.add align(formatFloat(n.pos.x * 100.0, precision = 11), 20)
  result.add align(formatFloat(n.pos.y * 100.0, precision = 11), 20)
  result.add align(formatFloat(n.pos.z * 100.0, precision = 11), 20)

func toStr(n: Node): string = $n

func voltStr(n: Node): string =
  result = &"{n.id:>8}{n.volt:>9.2f}"

func `<`(n, m: Node): bool = n.id < m.id

func almostEqual(x, y: float, eps = 1e-6): bool =
  abs(x - y) < eps

func checkNode(n: Node, x, y, z: float): bool =
  result = almostEqual(n.pos.x, x) and
           almostEqual(n.pos.y, y) and
           almostEqual(n.pos.z, z)

func toStr(e: Element): string =
  ## stringification as required in ELIST
  const numPerRow = 8
  result = &"    {e.id:>4}{e.matId:>4}   1   1   0   1    "
  var idx = 0
  let nNodes = e.nodeIds.len
  while idx < nNodes:
    for i in idx ..< min(idx + numPerRow, nNodes):
      result.add alignLeft($e.nodeIds[i], 8)
    idx += numPerRow
    if idx < nNodes:
      result.add "\n" & repeat(' ', 32)

func `<`(n, m: Element): bool = n.id < m.id

template check(f, buf, str: untyped, toRead: static bool = true): untyped =
  when toRead:
    doAssert f.readLine(buf)
  doAssert buf.startsWith(str), " line was: " & buf

proc parseNodes(f: FileStream): seq[Node] =
  const blkStr = "nblock,$i,,$i"
  const nodeLine = "$s$i$s$f$s$f$s$f"
  var buf: string
  # parse mesh file header manually
  check(f, buf, "/com")
  check(f, buf, "/wb")
  doAssert f.readLine(buf)
  var
    nBlk, nNodes: int
  if not buf.scanf(blkStr, nBlk, nNodes):
    raise newException(IOError, "Not a valid mesh file! `nblock` header could " &
      "not be matched.")
  check(f, buf, "(")
  var
    idx = 0
    nId = 0
    x, y, z: float
  result = newSeq[Node](nNodes)
  while f.readLine(buf) and idx < nNodes:
    doAssert buf.scanf(nodeLine, nId, x, y, z)
    result[idx] = Node(id: nId, pos: (x: x, y: y, z: z))
    inc idx
  # end of node <-> pos mapping
  check(f, buf, "-1", toRead = false)
  check(f, buf, "/wb,node,end")

proc parseElement(f: FileStream, buf: var string, id: int): Element =
  ## `id` is only given for cross check in debug mode
  var
    idx: int # idx within line, i.e. column idx
    num: int # current parsed number
    nNodes: int # number of nodes this element has
    i: int # number of parsed numbers
    nIdx: int # number of parsed nodes
  while idx < buf.len:
    idx += skipWhitespace(buf, start = idx)
    case i
    of 0:
      idx += parseInt(buf, num, start = idx)
      result.matId = num
    of 1:
      idx += parseInt(buf, num, start = idx)
      assert id == num, "expected id = " & $id & " but got num = " & $num
    of 2 .. 7, 9:
      idx += skipUntil(buf, ' ', start = idx)
    of 8:
      idx += parseInt(buf, num, start = idx)
      nNodes = num
      result.nodeIds = newSeq[int](num)
    of 10:
      idx += parseInt(buf, num, start = idx)
      result.id = num
    else:
      idx += parseInt(buf, num, start = idx)
      result.nodeIds[nIdx] = num
      inc nIdx
    inc i
  if nNodes > 8:
    # read next line as well
    idx = 0
    doAssert f.readLine(buf)
    while idx < buf.len:
      idx += skipWhitespace(buf, start = idx)
      idx += parseInt(buf, num, start = idx)
      result.nodeIds[nIdx] = num
      inc nIdx

proc parseElementGroup(f: FileStream, buf: var string): ElementGroup =
  const elementHeader = "/com,*********** Element Group$s$i"
  if not buf.scanf(elementHeader, result.num):
    raise newException(IOError, "Not a valid mesh file! `Element` header could " &
      "not be matched. Line was: " & buf)
  check(f, buf, "eblock,19,solid") # understand and parse!
  check(f, buf, "(19i8)") # understand and parse!
  var
    elements = newSeqOfCap[Element](100_000) # start reasonably big
  while f.readLine(buf) and not buf.startsWith("-1"):
    elements.add parseElement(f, buf, result.num)
  result.elements = elements

proc parseElementGroups(f: FileStream): seq[ElementGroup] =
  var buf: string
  check(f, buf, "/wb,elem,start")
  while f.readLine(buf) and not buf.startsWith("/wb,elem,end"):
    # we have no clue about how many elements a group contains afaiu
    result.add parseElementGroup(f, buf)

proc parseMeshFile(path: string): MeshFile =
  var f = newFileStream(path / defaultNodeFile)
  doAssert not f.isNil
  result.nodes = parseNodes f
  result.groups = parseElementGroups f
  f.close()

proc parsePotentialFile(path: string, nodes: var seq[Node]) =
  ## parses the file containing the potential values for each node and fills
  ## the `volt` field of each node.
  ## The potential.csv file is exported via the user interface of discovery aim.
  ## Thus, it does not give the node number, but rather the position of the node.
  ## They however are sorted according to their node number. To see this, compare
  ## the positions with the node number / position pairs in `mesh.dat` to the
  ## positions found in this file. Alternatively compile with `-d:checkNodes` to
  ## perform runtime checks for each row.
  ##
  ## NOTE: The input nodes have to be sorted according to their node id!
  var f = newFileStream(path / defaultPotentialFile)
  doAssert not f.isNil
  var
    buf: string
    idx: int
    x, y, z, volt: float
  check(f, buf, "X [m], Y [m], Z [m], ElectricPotential [kg m^2 A^-1 s^-3]")
  while f.readLine(buf):
    doAssert buf.scanf("$f,$s$f,$s$f,$s$f", x, y, z, volt)
    when defined(checkNodes):
      doAssert checkNode(nodes[idx], x, y, z)
      doAssert nodes[idx].id == idx + 1
    nodes[idx].volt = volt
    inc idx
  f.close()

proc writeListFile[T](args: seq[T], outfile: string,
                      numPerTab: int,
                      header, tabHeader: string,
                      print: (proc(x: T): string)) =
  template writeTab(f: File, n, nIdx, num: untyped): untyped =
    f.write("\n")
    f.write(tabHeader & "\n")
    for idx in 0 ..< num:
      f.write(print(n[nIdx + idx]) & "\n")

  var f = open(outfile, fmWrite)
  f.write(header & "\n")
  let
    nFull = args.len div numPerTab
    nRest = args.len mod numPerTab
  for i in 0 ..< nFull:
    writeTab(f, args, i * numPerTab, numPerTab)
  # write the remaining
  if nRest > 0:
    writeTab(f, args, args.high - nRest, nRest)

  f.close()

proc writeNLIST(nodes: seq[Node], outpath: string) =
  ## writes the nodes to the `NLIST.lis` file as expected by garfield
  ##
  ## file layout:
  ## - header consists of empty line, one LIST ... line
  ## - tables: empty line, table header (NODE, X, Y, Z), 20 nodes
  const numPerTab = 20
  const header = "\n LIST ALL SELECTED NODES.   DSYS=      0"
  # TODO: find out if it matters whether there is more / less space than the
  # weird `NLIST.lis` (3, 4, 9, 9) spaces...
  let tabHeader = ["NODE", "X", "Y", "Z"].mapIt(it.align(20)).join()
  writeListFile(nodes, outpath / "NLIST.lis", numPerTab, header, tabHeader, toStr)

proc writeELIST(elements: seq[Element], outpath: string) =
  ## writes the elements to the `ELIST.lis` file as expected by garfield
  ##
  ## file layout:
  ## - header consists of empty line, one LIST ... line
  ## - tables: empty line, table header (NODE, X, Y, Z), 10 elements,
  ##   possibly multiple lines each
  const numPerTab = 10
  const header = "\n  LIST ALL SELECTED ELEMENTS.  (LIST NODES)"
  let tabHeader = ["", "ELEM", "MAT", "TYP", "REL", "ESY", "SEC", "", ""]
    .mapIt(it.align(4)).join() & "NODES\n"
  writeListFile(elements, outpath / "ELIST.lis", numPerTab, header, tabHeader, toStr)

proc writePRNSOL(nodes: seq[Node], outpath: string) =
  ## writes the voltages at each node to the `PRNSOL.lis` file as expected by
  ## garfield.
  ##
  ## file layout:
  ## - header: consists of empty line, one PRINT ... line
  ## - tables: multi (6, 4 with text) line table header, 39 nodes in each
  const numPerTab = 39
  const header = "\n PRINT VOLT NODAL SOLUTION PER NODE"
  let tabHeader = """

  ***** POST1 NODAL DEGREE OF FREEDOM LISTING *****

  LOAD STEP=     1  SUBSTEP=     1
   TIME=    1.0000      LOAD CASE=   0

    NODE       VOLT
"""
  writeListFile(nodes, outpath / "PRNSOL.lis", numPerTab, header, tabHeader, voltStr)

proc main(path: string, outpath = ".") =
  var mesh = parseMeshFile(path)
  echo "parsing of ", mesh.nodes.len, " nodes done"
  mesh.nodes.sort() # sort by id
  writeNLIST(mesh.nodes, outpath)

  let elements = concat(mesh.groups.mapIt(it.elements)).sorted
  writeELIST(elements, outpath)

  parsePotentialFile(path, mesh.nodes)
  writePRNSOL(mesh.nodes, outpath)

when isMainModule:
  dispatch main
