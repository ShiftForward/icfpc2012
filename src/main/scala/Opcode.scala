sealed trait Opcode
case class Up() extends Opcode
case class Down() extends Opcode
case class Left() extends Opcode
case class Right() extends Opcode
case class Wait() extends Opcode
case class Abort() extends Opcode

object VM {
  def eval(o: Opcode, b: Board): Board = {
    val tentativePos = (o match {
      case Up    ⇒ b.robot.position.Up
      case Down  ⇒ b.robot.position.Down
      case Left  ⇒ b.robot.position.Left
      case Right ⇒ b.robot.position.Right
      case Wait  ⇒ b.robot.position
    })

    // Ugly non-functional code. If bot tries to move outside board, it remains in the same place.
    val newPos = if (b.contains(tentativePos)) tentativePos else b.robot.position

    b.copy(tiles = b.tiles map { _ match {
      case e: Earth  if e.pos == newPos ⇒ Empty(newPos)
      case l: Lambda if l.pos == newPos ⇒ Empty(newPos)
      case w: Wall ⇒ w
    } })
  }
}
