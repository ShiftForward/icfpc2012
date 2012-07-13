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
      case _        => b.robotPos
    }

    println ("Move from " + b.robotPos + " to " + tentativePos)

    val newPos  = if (b.contains(tentativePos)) {
      b.get(tentativePos) match {
        case _: Empty | _: Earth => tentativePos
        case otherwise           => b.robotPos
      }
    } else b.robotPos

    val updatedBoardStep1 = b.tiles + (b.robotPos -> Empty()) + (newPos -> Robot())

    val updatedBoard = b.empty ++ (updatedBoardStep1 map { case (pos, tile) =>
      tile match {
        case _: Rock => b.get(pos.Down) match {
          case _: Empty => (pos.Down -> FallingRock())
          case _: Rock  if (b.get(pos.Down.Right).isInstanceOf[Empty]) => (pos.Down.Right -> FallingRock())
          case _: Rock  if (b.get(pos.Down.Left).isInstanceOf[Empty])  => (pos.Down.Left  -> FallingRock())
          case _        => (pos -> StableRock())
        }
        case _ => (pos -> tile)
      }
    })

    PlayingBoard(b.width, b.height, updatedBoard, newPos)
  }
}
