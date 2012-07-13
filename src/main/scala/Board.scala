import scala.io.Source
import scala.collection.immutable.TreeMap

import Coordinate.Implicits._

sealed trait Board {
  def width: Int
  def height: Int
  def tiles: Map[Coordinate, Tile]
  def robotPos: Coordinate

  def contains(pos: Coordinate) = pos.isInside(width, height)
  def get(pos: Coordinate): Tile = tiles.get(pos).getOrElse(Invalid())

  override def toString = {
    val lines = TreeMap(tiles.toArray: _*).groupBy { case (pos, _) => pos.y }
    val sortedLines = TreeMap(lines.toArray: _*)

    sortedLines.map { case (n, line) =>
      line.map { case (_, tile) =>
        tile match {
          case _: Robot => 'R'
          case _: Wall => '#'
          case _: Lambda => '\\'
          case _: Earth => '.'
          case _: Empty => ' '
          case _: ClosedLift => 'L'
          case _: OpenLift => 'O'
          case _: Rock => '*'
          case _ => '?'
        }
      }.mkString

    }.mkString("\n")
  }
}

case class LostBoard(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate) extends Board
case class WonBoard(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate) extends Board
case class PlayingBoard(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate) extends Board

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

  def apply(): Board = apply(Source.stdin.getLines().takeWhile(_ != "").toSeq)
}
