import strscans, strutils, streams, os
import cligen

## The node information by default should be contained in the
## mesh.dat file
const defaultNodeFile = "mesh.dat"

type
  Node = object
    id: int
    pos: tuple[x, y, z: float]

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
  echo nNodes
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

proc main(path: string) =
  # parse the nodes
  let nodes = parseNodes(path)


when isMainModule:
  dispatch main
