import strscans, strutils, streams, os, sequtils, algorithm, parseutils
import cligen

## The node information by default should be contained in the
## mesh.dat file
const defaultNodeFile = "mesh.dat"

type
  Node = object
    id: int
    pos: tuple[x, y, z: float]

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
  result = newStringOfCap(80)
  result.add align($n.id, 20)
  result.add align(formatFloat(n.pos.x, precision = 11), 20)
  result.add align(formatFloat(n.pos.y, precision = 11), 20)
  result.add align(formatFloat(n.pos.z, precision = 11), 20)

func `<`(n, m: Node): bool = n.id < m.id

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
  template writeTab(f, nIdx, n, num: untyped): untyped =
    f.write("\n")
    f.write(tabHeader & "\n")
    for idx in 0 ..< num:
      f.write($n[nIdx + idx] & "\n")

  var f = open(outpath / "NLIST.lis", fmWrite)
  f.write(header & "\n")
  let
    nFull = nodes.len div numPerTab
    nRest = nodes.len mod numPerTab
  for i in 0 ..< nFull:
    writeTab(f, i * numPerTab, nodes, numPerTab)
  # write the remaining
  if nRest > 0:
    writeTab(f, nodes.high - nRest, nodes, nRest)

  f.close()

proc main(path: string, outpath = ".") =
  var mesh = parseMeshFile(path)
  echo "parsing of ", mesh.nodes.len, " nodes done"
  mesh.nodes.sort() # sort by id
  writeNLIST(mesh.nodes, outpath)


when isMainModule:
  dispatch main
