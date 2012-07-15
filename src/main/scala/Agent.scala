object Agent {
  def getMoves(b: Board) = {
    val moves = AStar.evaluateBestSolution(b)

    val resultingBoard = moves.foldLeft(b) { (board, move) => board.eval(move) }
    if (resultingBoard.lambdas != resultingBoard.tLambdas)
      moves ++ List('Abort)
    else
      moves
  }
}
