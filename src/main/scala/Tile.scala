import scala.io.Source

import Coordinate.Implicits._

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
  def apply(board: Seq[String]): Board = {
    val width = board.head.length
    val height = board.length

    val tiles = board.zipWithIndex.map { case (line, y) =>
      line.zipWithIndex.map { case (char, x) =>
        ((x, y): Coordinate, Tile(char))
      }
    }.flatten

    val robotPos = tiles.find { case (_, tile) =>
      tile match {
        case _: Robot => true
        case _ => false
      }
    }.map(_._1).get

    new PlayingBoard(width, height, tiles.toMap, robotPos)
  }

  def apply(): Board = apply(Source.stdin.getLines.takeWhile(_ != "").toSeq)
}


sealed trait Board {
  def width: Int
  def height: Int
  def tiles: Map[Coordinate, Tile]
  def robotPos: Coordinate

  def contains(pos: Coordinate) = pos.isInside(width, height)
  def get(pos: Coordinate): Tile = tiles.get(pos).getOrElse(Invalid())

  def toString = {

  }
}

case class LostBoard(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate) extends Board
case class WonBoard(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate) extends Board
case class PlayingBoard(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate) extends Board
