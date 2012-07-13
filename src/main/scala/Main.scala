// ****************************************************************
// *   Software Failure.  Press left mouse button to continue.    *
// *             Guru Meditation #00000000.00000000               *
// ****************************************************************

object Main extends App {
  val b = Board("#*. #\n# R #\n#####".split('\n'))

  val x0 = VM.eval(Right(), b)
  val x1 = VM.eval(Right(), x0)
  val x2 = VM.eval(Left(), x1)
  val x3 = VM.eval(Left(), x2)

  println (x3.toString)
}
