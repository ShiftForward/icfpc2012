// ****************************************************************
// *   Software Failure.  Press left mouse button to continue.    *
// *             Guru Meditation #00000000.00000000               *
// ****************************************************************

import Opcode._

object Main extends App {
  def time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      println("Elapsed time: " + (t1 - t0) / 1000000 + " milis")
      result
  }

  scala.sys.addShutdownHook {
    println("Received SIGINT! 10 more seconds to go.")
  }

  // val b = Board("#*. #\n# R\\#\n#####".split('\n'))
  // val b = Board("#R\\ #\n#* *#\n#* *#\n#####".split('\n'))
  // val b = Board("#L#\n#*#\n#*#\n# #\n# #\n#R#\n#\\#\n###".split('\n'))
  // val b = Board("#L#\n#*#\n#*#\n# #\n# #\n#R#\n#\\#\n###".split('\n'))
  // val b = Board("# #\n#L#\n#\\#\n#R#\n###".split('\n'))

  //val b = Board.create("#  #\n#  #\n#  #\n#R #\n# *#\n# *#".split('\n'), (-1, 2, 1))
  val b = Board("src/main/resources/map/contest8.map")
  println(b)

  val lambdas = b.allLambdas
  var moves: List[Opcode] = null
  val abortList = List('Abort)

  time {
    moves = Agent.visitNodes(lambdas, b)

    val resb = moves.foldLeft(b) { (cb: Board, m: Opcode) => cb.eval(m) }
    if (resb.lambdas == resb.tLambdas) {
      val movesToEnd = ShortestPathCalculator.shortestPath(resb.liftPosition, resb)
      moves ++= (if (!movesToEnd.isEmpty) movesToEnd else abortList)
    } else {
      moves ++= abortList
    }

    println (resb)
  }

  println (Opcode.toString(moves))
}
