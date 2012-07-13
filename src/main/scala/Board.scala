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
  lazy val sortedKeys = for (i <- Range(0, width); j <- Range(height-1, -1, -1)) yield Coordinate(i, j)

  def eval(o: Opcode): Board = this

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
case class PlayingBoard(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate, lambdas: Int = 0) extends Board {
  override def eval(o: Opcode): Board = {
    var lambdas = 0

    val existsLambdas = this.tiles.exists(_._2.isInstanceOf[Lambda])

    val tentativePos = o match {
      case _: MoveUp    => robotPos.Up
      case _: MoveDown  => robotPos.Down
      case _: MoveLeft  => robotPos.Left
      case _: MoveRight => robotPos.Right
      case _            => robotPos
    }

    println ("Move from " + robotPos + " to " + tentativePos)

    val newPos = if (this.contains(tentativePos)) {
      this.get(tentativePos) match {
        case _: Empty | _: Earth => tentativePos
        case _: Lambda =>
          lambdas += 1
          tentativePos
        case _ => robotPos
      }
    } else robotPos

    println ("Conclusion is " + newPos)

    val robotMove = PlayingBoard(width, height, tiles + (robotPos -> Empty()) + (newPos -> Robot()), newPos, this.lambdas)
    val updatedBoard = robotMove.empty ++ (sortedKeys collect { case pos if !robotMove.get(pos).isInstanceOf[Empty] =>
      robotMove.get(pos) match {
        case _: Rock => robotMove.get(pos.Down) match {
          case _: Empty => println("Pos = " + pos); (pos.Down -> FallingRock())
          case _: Rock  if (robotMove.get(pos.Down.Right).isInstanceOf[Empty]) => (pos.Down.Right -> FallingRock())
          case _: Rock  if (robotMove.get(pos.Down.Left).isInstanceOf[Empty])  => (pos.Down.Left  -> FallingRock())
          case _        => (pos -> StableRock())
        }
        case _: ClosedLift if !existsLambdas => (pos -> OpenLift())
        case w => (pos -> w)
      }
    })

    if (updatedBoard.filter(_._2.isInstanceOf[FallingRock]).exists(_._1.Down == newPos))
      LostBoard(width, height, updatedBoard, newPos, this.lambdas + lambdas)
    else
      PlayingBoard(width, height, updatedBoard, newPos, this.lambdas + lambdas)
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

  def apply(): Board = apply(Source.stdin.getLines().takeWhile(_ != "").toSeq)
}
