// ****************************************************************
// *   Software Failure.  Press left mouse button to continue.    *
// *             Guru Meditation #00000000.00000000               *
// ****************************************************************

object Main extends App {
  scala.sys.addShutdownHook {
    println("Received SIGINT! 10 more seconds to go.")
  }

  val b = Board("#*. #\n# R\\#\n#####".split('\n'))
  val f = b.eval(MoveUp()).eval(MoveDown()).eval(MoveRight()).eval(MoveLeft())

  println (f)
  println ("You caught: " + f.lambdas)
}
