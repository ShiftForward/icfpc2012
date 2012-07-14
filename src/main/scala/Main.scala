// ****************************************************************
// *   Software Failure.  Press left mouse button to continue.    *
// *             Guru Meditation #00000000.00000000               *
// ****************************************************************

object Main extends App {
  scala.sys.addShutdownHook {
    println("Received SIGINT! 10 more seconds to go.")
  }

  // val b = Board("#*. #\n# R\\#\n#####".split('\n'))
  // val b = Board("#R\\ #\n#* *#\n#* *#\n#####".split('\n'))
  // val b = Board("#L#\n#*#\n#*#\n# #\n# #\n#R#\n#\\#\n###".split('\n'))
  // val b = Board("#L#\n#*#\n#*#\n# #\n# #\n#R#\n#\\#\n###".split('\n'))
  // val b = Board("# #\n#L#\n#\\#\n#R#\n###".split('\n'))

  //val b = Board.create("#  #\n#  #\n#  #\n#R #\n# *#\n# *#".split('\n'), (-1, 2, 1))
  val b = Board("src/main/resources/map/contest1.map")
  println(b)

  val lambdas = b.allLambdas

  var moves = Agent.visitNodes(lambdas, b)

  // val b = Board("#RL#\n#* #\n#\\ #\n#  #\n#  #".split('\n'))

//  b.printStatus()

//  val moves = List(Wait(), MoveDown(), MoveDown(), Wait(), MoveUp(), MoveUp(), MoveRight(), MoveUp(), MoveUp(), MoveUp())
  // val moves = List(MoveDown(), MoveDown(), MoveDown(), Wait(), Wait(), Wait(), Wait(), Wait(), Wait(), Wait())

  val resb = moves.foldLeft(b) { (cb: Board, m: Opcode) => cb.eval(m) }
  if (resb.lambdas == resb.tLambdas) {
    val movesToEnd = ShortestPathCalculator.shortestPath(resb.liftPosition, resb)
    if (!movesToEnd.isEmpty)
      moves ++= movesToEnd
    else
      moves ++= List(Abort())
  } else {
    moves ++= List(Abort())
  }
  println(resb)
  moves.foreach(print)
  println
}
