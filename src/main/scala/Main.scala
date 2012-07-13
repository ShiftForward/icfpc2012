// ****************************************************************
// *   Software Failure.  Press left mouse button to continue.    *
// *             Guru Meditation #00000000.00000000               *
// ****************************************************************

object Main extends App {
  scala.sys.addShutdownHook {
    println("Received SIGINT! 10 more seconds to go.")
  }

  // val b = Board("#*. #\n# R\\#\n#####".split('\n'))
  val b = Board("#R  #\n#* *#\n#* *#\n#####".split('\n'))
  // val b = Board("#L#\n#*#\n#*#\n# #\n# #\n#R#\n#\\#\n###".split('\n'))
  // val b = Board("#L#\n#*#\n#*#\n# #\n# #\n#R#\n#\\#\n###".split('\n'))
  // val b = Board("# #\n#L#\n#\\#\n#R#\n###".split('\n'))

  // val b = Board("#R #\n# *#\n# *#".split('\n'))

  // val b = Board("#R#\n#*#\n# #\n# #".split('\n'))

  println (b)

  println("--- AFTER ----")

  val f = b.eval(MoveDown()).eval(MoveDown())

  println (f)
  println("Status is " + f.getClass.toString + " and you caught " + f.lambdas + "λ")


  // println ("You caught: " + f.lambdas)
}
