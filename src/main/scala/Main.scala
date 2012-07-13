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

  // val b = Board("#R #\n# *#\n# *#".split('\n'))

  val b = Board("#RL#\n#* #\n#\\ #\n#  #\n#  #".split('\n'))

  println(b)
  println(b.robotPos)
  println("--- AFTER ----")

  val f = b.eval(Wait()).eval(MoveDown()).eval(MoveDown()).eval(MoveUp()).eval(MoveUp()).eval(MoveRight())

  println(f)
  println(f.robotPos)
  println("Status is " + f.getClass.toString + " and you caught " + f.lambdas + " lambdas of " + f.tLambdas + " lambdas hence you('re) " + f.status)


  // println ("You caught: " + f.lambdas)
}
