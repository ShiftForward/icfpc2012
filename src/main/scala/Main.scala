// ****************************************************************
// *   Software Failure.  Press left mouse button to continue.    *
// *             Guru Meditation #00000000.00000000               *
// ****************************************************************

object Main extends App {
  val b = Board("#*. #\n# R\\#\n#####".split('\n'))

  val x0 = VM.eval(Up(), b)
  val x1 = VM.eval(Down(), x0)
  val x2 = VM.eval(Right(), x1)
  val x3 = VM.eval(Left(), x2)

  println (x3.toString)
  println ("You caught: " + x3.lambdas)
}
