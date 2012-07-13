// ****************************************************************
// *   Software Failure.  Press left mouse button to continue.    *
// *             Guru Meditation #00000000.00000000               *
// ****************************************************************

object Main extends App {
//  val b = Board("#*. #\n# R\\#\n#####".split('\n'))
  val b = Board("#R  #\n#* *#\n#* *#\n#####".split('\n'))

  val f = b.eval(Up()).eval(Down()).eval(Right()).eval(Left())

  println (f)
  println ("You caught: " + f.lambdas)
}
