sealed case class Tile()
case class Robot() extends Tile
case class Wall() extends Tile
case class Lambda() extends Tile
sealed case class Reachable() extends Tile
case class Earth() extends Reachable
case class Empty() extends Reachable
case class Invalid() extends Tile

sealed case class Lift() extends Tile
case class OpenLift() extends Tile
case class ClosedLift() extends Lift

sealed case class Rock() extends Tile
case class StableRock() extends Rock
case class FallingRock() extends Rock

object Tile {
  def apply(c: Char): Tile = {
    c match {
      case 'R'  => Robot()
      case '#'  => Wall()
      case '*'  => StableRock()
      case '\\' => Lambda()
      case 'L'  => ClosedLift()
      case 'O'  => OpenLift()
      case '.'  => Earth()
      case ' '  => Empty()
    }
  }
}
