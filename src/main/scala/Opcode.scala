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

    // Ugly non-functional code. If bot tries to move outside board, it remains in the same place.
    val newPos = if (b.contains(tentativePos)) tentativePos else b.robotPos

    val updatedBoardStep1 = b.tiles + (b.robotPos -> Empty()) ++ ((b.get(newPos) match {
      case _: Empty | _: Earth | _: Robot => Map(newPos -> Robot(), b.robotPos -> Empty())
      case otherwise => Map.empty[Coordinate, Tile]
    }))

    val updatedBoard = updatedBoardStep1 map { case (pos, tile) =>
      tile match {
        case _: Rock => pos.Down match {
          case _: Empty => (pos.Down -> FallingRock())
          case _: Rock  if (pos.Down.Right.isInstanceOf[Empty]) => (pos.Down.Right -> FallingRock())
          case _: Rock  if (pos.Down.Left.isInstanceOf[Empty])  => (pos.Down.Left  -> FallingRock())
          case _        => (pos -> StableRock())
        }
        case _ => (pos -> tile)
      }
    }

    PlayingBoard(b.width, b.height, updatedBoard, newPos)
  }
}
