// ****************************************************************
// *   Software Failure.  Press left mouse button to continue.    *
// *             Guru Meditation #00000000.00000000               *
// ****************************************************************

object Main extends App {
  scala.sys.addShutdownHook {
    println("Received SIGINT! 10 more seconds to go.")
  }

  // val b = Board("#*. #\n# R\\#\n#####".split('\n'))
  // val b = Board("#R  #\n#* *#\n#* *#\n#####".split('\n'))
  val b = Board("#L#\n#*#\n#*#\n# #\n# #\n#R#\n#\\#\n###".split('\n'))
  println (b)

  val f = b.eval(MoveDown()).eval(Wait())

  println (f)
  println("Status is " + f.isInstanceOf[LostBoard] + " and you caught " + f.lambdas)

  // println ("You caught: " + f.lambdas)
}
