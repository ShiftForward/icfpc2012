object Agent {
  def visitNodes(nodes: List[Coordinate], board: Board): List[Opcode] = {
    if (nodes.isEmpty)
      List()
    else {
      val ops = ShortestPathCalculator.shortestPath(nodes.head, board)
      if (ops.length > 0) {
        val nextBoard = board.eval(ops)
        ops ++ visitNodes(nodes.tail, nextBoard)
      } else {
        visitNodes(nodes.tail, board)
      }
    }
  }
}
