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
  val b = Board("#*#\n#*#\n# #\n# #\n#R#".split('\n'))
  println (b)

  val f = b.eval(Wait()).eval(Wait()).eval(Wait()).eval(Wait())

  println (f)
  println("Status is " + f.isInstanceOf[LostBoard])

  // println ("You caught: " + f.lambdas)
}
