import scala.io.Source

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
  def apply(c: Char, pos: Coordinate): Tile = {
    c match {
      case 'R' => Robot()
      case '#' => Wall()
      case '*' => {
        // FIXME: check if rock is falling or not
        StableRock()
      }
      case '\\' => Lambda()
      case 'L' => ClosedLift()
      case 'O' => OpenLift()
      case '.' => Earth()
      case ' ' => Empty()
    }
  }
}

object Board {
  // def apply(board: String): Board = {
  // }

  // def apply() = apply(S
}


sealed trait Board {
  def width: Int
  def height: Int
  def tiles: Map[Coordinate, Tile]
  def robotPos: Coordinate

  def contains(pos: Coordinate) = pos.isInside(width, height)
  def get(pos: Coordinate): Tile = tiles.get(pos).orElse(Invalid())
}

case class LostBoard(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate) extends Board
case class WonBoard(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate) extends Board
case class PlayingBoard(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate) extends Board
