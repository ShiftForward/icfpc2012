object Agent {
  def orderedDestinations(c: Coordinate, destinations: List[Coordinate]) = {
    destinations.sortBy { coordinate => coordinate.distance(c) }
  }

  def visitNodes(nodes: List[Coordinate], board: Board): List[Opcode] = {
    if (nodes.isEmpty)
      List()
    else {
      val ops = ShortestPathCalculator.shortestPath(nodes.head, board)
      if (ops.length > 0) {
        println("Got from " + board.robotPos + " to " + nodes.head)
        val nextBoard = board.eval(ops)
        ops ++ visitNodes(orderedDestinations(nextBoard.robotPos, nodes.tail),
                          nextBoard)
      } else {
        println("Unable to get from " + board.robotPos + " to " + nodes.head)
        visitNodes(nodes.tail, board)
      }
    }
  }
}
