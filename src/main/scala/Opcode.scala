sealed trait Opcode
case class Up() extends Opcode
case class Down() extends Opcode
case class Left() extends Opcode
case class Right() extends Opcode
case class Wait() extends Opcode
case class Abort() extends Opcode

object VM {
  def eval(o: Opcode, b: Board): Board = {
    val tentativePos = o match {
      case _: Up    => b.robotPos.Up
      case _: Down  => b.robotPos.Down
      case _: Left  => b.robotPos.Left
      case _: Right => b.robotPos.Right
      case _: Wait  => b.robotPos
    }

    // Ugly non-functional code. If bot tries to move outside board, it remains in the same place.
    val newPos = if (b.contains(tentativePos)) tentativePos else b.robotPos

    b.copy(tiles = b.tiles map { case (pos, tile) =>

      tile match {
        case e: Earth  if pos == newPos => (pos -> Empty())
        case l: Lambda if pos == newPos => (pos -> Empty())
        case w: Wall => (pos -> w)
      }
    })
  }
}
