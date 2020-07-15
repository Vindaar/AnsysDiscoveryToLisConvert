import strscans, strutils, streams, os, sequtils, algorithm
import cligen

## The node information by default should be contained in the
## mesh.dat file
const defaultNodeFile = "mesh.dat"

type
  Node = object
    id: int
    pos: tuple[x, y, z: float]

func `$`(n: Node): string =
  result = newStringOfCap(80)
  result.add align($n.id, 20)
  result.add align(formatFloat(n.pos.x, precision = 11), 20)
  result.add align(formatFloat(n.pos.y, precision = 11), 20)
  result.add align(formatFloat(n.pos.z, precision = 11), 20)

func `<`(n, m: Node): bool = n.id < m.id

proc parseNodes(path: string): seq[Node] =
  const blkStr = "nblock,$i,,$i"
  const nodeLine = "$s$i$s$f$s$f$s$f"

  var f = newFileStream(path / defaultNodeFile)
  var buf: string
  # parse mesh file header manually
  doAssert f.readLine(buf)
  assert buf.startsWith("/com")
  doAssert f.readLine(buf)
  assert buf.startsWith("/wb")
  doAssert f.readLine(buf)
  var
    nBlk, nNodes: int
  if not buf.scanf(blkStr, nBlk, nNodes):
    raise newException(IOError, "Not a valid mesh file! `nblock` header could " &
      "not be matched.")
  doAssert f.readLine(buf)
  assert buf.startsWith("(")
  var
    idx = 0
    nId = 0
    x, y, z: float
  result = newSeq[Node](nNodes)
  while f.readLine(buf) and idx < nNodes:
    doAssert buf.scanf(nodeLine, nId, x, y, z)
    result[idx] = Node(id: nId, pos: (x: x, y: y, z: z))
    inc idx
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
  var nodes = parseNodes(path)
  echo "parsing of ", nodes.len, " nodes done"
  nodes.sort() # sort by id
  writeNLIST(nodes, outpath)


when isMainModule:
  dispatch main
