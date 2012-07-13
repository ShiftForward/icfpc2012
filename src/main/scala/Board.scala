import scala.io.Source
import scala.collection.immutable.TreeMap

import Coordinate.Implicits._

sealed trait Board {
  def width: Int
  def height: Int
  def tiles: Map[Coordinate, Tile]
  def robotPos: Coordinate
  def lambdas: Int

  def contains(pos: Coordinate) = pos.isInside(width, height)
  def get(pos: Coordinate): Tile = tiles.get(pos).getOrElse(Invalid())

  lazy val empty = (for (i <- Range(0, width); j <- Range(0, height)) yield (Coordinate(i, j) -> Empty())).toMap[Coordinate, Tile]

  def eval(o: Opcode): Board = {
    val b = this
    var lambdas = 0

    val tentativePos = o match {
      case _: MoveUp    => b.robotPos.Up
      case _: MoveDown  => b.robotPos.Down
      case _: MoveLeft  => b.robotPos.Left
      case _: MoveRight => b.robotPos.Right
      case _            => b.robotPos
    }

    println ("Move from " + b.robotPos + " to " + tentativePos)

    val newPos = if (b.contains(tentativePos)) {
      b.get(tentativePos) match {
        case _: Empty | _: Earth => tentativePos
        case _: Lambda =>
          lambdas += 1
          tentativePos
        case _ => b.robotPos
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

    if (updatedBoard.filter(_._2.isInstanceOf[FallingRock]).exists(_._1.Down ==
    ))
      LostBoard(b.width, b.height, updatedBoard, newPos, b.lambdas + lambdas)
    else
      PlayingBoard(b.width, b.height, updatedBoard, newPos, b.lambdas + lambdas)
  }

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

  def iterator = TreeMap(tiles.toArray: _*).iterator
}

case class LostBoard(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate, lambdas: Int = 0) extends Board
case class WonBoard(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate, lambdas: Int = 0) extends Board
case class PlayingBoard(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate, lambdas: Int = 0) extends Board

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
