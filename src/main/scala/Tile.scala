sealed trait Tile

sealed trait Reachable
sealed trait Unreachable

case class Robot() extends Tile
case class Wall() extends Tile with Unreachable
case class Lambda() extends Tile with Reachable
case class Earth() extends Tile with Reachable
case class Empty() extends Tile with Reachable
case class Invalid() extends Tile with Unreachable

sealed trait Lift extends Tile
case class OpenLift() extends Lift with Reachable
case class ClosedLift() extends Lift with Reachable

sealed trait Rock extends Tile with Reachable
case class StableRock() extends Rock
case class FallingRock() extends Rock

object Tile {
  def apply(c: Char): Tile = {
    c match {
      case 'R' => Robot()
      case '#' => Wall()
      case '*' => StableRock()
      case '\\' => Lambda()
      case 'L' => ClosedLift()
      case 'O' => OpenLift()
      case '.' => Earth()
      case ' ' => Empty()
    }
  }
}
