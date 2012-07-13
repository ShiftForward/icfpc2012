import akka.pattern.Patterns
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

case class Pattern(opCode: Opcode, source: Map[(Int, Int), Tile], dest: Map[(Int, Int), Tile]) {
  def isMatch(b: Board, pos: Coordinate) =
    source.forall(hp => hp._2.getClass.isAssignableFrom(b.get(pos + Coordinate(hp._1._1, hp._1._2)).getClass))

  def replace(b: Board, pos: Coordinate): Map[Coordinate, Tile] =
    dest.map(hp => ((pos + Coordinate(hp._1._1, hp._1._2))) -> hp._2)
}

case class LostBoard(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate, lambdas: Int = 0) extends Board
case class WonBoard(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate, lambdas: Int = 0) extends Board

case class PlayingBoard(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate, lambdas: Int = 0) extends Board {
  def applyPatterns(b: Board, opCode: Opcode, patterns: List[Pattern]): Board = {
    val ts = b.sortedKeys flatMap { pos =>
      patterns.filter(_.opCode.getClass.isAssignableFrom(opCode.getClass)) collect { case p if (p.isMatch(b, pos)) => p.replace(b, pos) }
    }

    ts.foldLeft(b)((acc, t) => PlayingBoard(b.width, b.height, acc.tiles ++ t, b.robotPos, b.lambdas))
  }

  override def eval(o: Opcode): Board = {
    import Board._

    val newBoardA = applyPatterns(this, o, List(MvRight, MvLeft, MvUp, MvDown, PushRight, PushLeft))
    applyPatterns(newBoardA, o, List(Fall, FallRight, FallLeft, Die))
 }
}

object Board {
  val PushRight = Pattern(MoveRight(),
                          Map((0, 0) -> Robot(), (1, 0) -> StableRock(), (2, 0) -> Empty()),
                          Map((0, 0) -> Empty(), (1, 0) -> Robot(), (2, 0) -> StableRock()))

  val PushLeft  = Pattern(MoveRight(),
                          Map((0, 0) -> Empty(), (1, 0) -> StableRock(), (2, 0) -> Robot()),
                          Map((0, 0) -> StableRock(), (1, 0) -> Robot(), (2, 0) -> Empty()))

  val Fall      = Pattern(Opcode(),
                          Map((0, 0) -> Rock(),  (0, 1) -> Empty()),
                          Map((0, 0) -> Empty(), (0, 1) -> FallingRock()))

  val FallRight = Pattern(Opcode(),
                          Map((0, 0) -> Rock(),  (1, 0) -> Empty(), (0, 1) -> Rock(), (1, 1) -> Empty()),
                          Map((0, 0) -> Empty(), (1, 0) -> Empty(), (0, 1) -> Rock(), (1, 1) -> FallingRock()))

  val FallLeft  = Pattern(Opcode(),
                          Map((0, 0) -> Empty(), (1, 0) -> Rock(), (0, 1) -> Empty(), (1, 1) -> Rock()),
                          Map((0, 0) -> Empty(), (1, 0) -> Empty(), (0, 1) -> FallingRock(), (1, 1) -> Rock()))

  val MvRight   = Pattern(MoveRight(),
                          Map((0, 0) -> Robot(), (1, 0) -> Reachable()),
                          Map((0, 0) -> Empty(), (1, 0) -> Robot()))

  val MvLeft    = Pattern(MoveLeft(),
                          Map((0, 0) -> Reachable(), (1, 0) -> Robot()),
                          Map((0, 0) -> Robot(), (1, 0) -> Empty()))

  val MvUp      = Pattern(MoveUp(),
                          Map((0, 0) -> Reachable(), (0, 1) -> Robot()),
                          Map((0, 0) -> Robot(), (0, 1) -> Empty()))

  val MvDown    = Pattern(MoveDown(),
                          Map((0, 0) -> Robot(), (0, 1) -> Reachable()),
                          Map((0, 0) -> Empty(), (0, 1) -> Robot()))

  val Die       = Pattern(MoveDown(),
                          Map((0, 0) -> FallingRock(), (0, 1) -> Robot()),
                          Map((0, 0) -> Empty(), (0, 1) -> DeadRobot()))

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
