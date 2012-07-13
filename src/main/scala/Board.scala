import scala.io.Source
import scala.collection.immutable.TreeMap

import Coordinate.Implicits._

case class Pattern(pred: (Board, Opcode) => Boolean, source: Map[(Int, Int), Tile], dest: Map[(Int, Int), Tile], f: (Board => Board) = identity ) {
  def isMatch(b: Board, pos: Coordinate) =
    source.forall(hp => hp._2.getClass.isAssignableFrom(b.get(pos + Coordinate(hp._1._1, hp._1._2)).getClass))

  def replace(b: Board, pos: Coordinate): Map[Coordinate, Tile] =
    dest.map(hp => ((pos + Coordinate(hp._1._1, hp._1._2))) -> hp._2)
}

case class Board(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate, water: Int = -1, flooding: Int = 0,
                 waterProof: Int = 10, tick: Int = 0, lambdas: Int = 0, status: Status = Playing(), tLambdas: Int = 0, ticksUnderwater: Int = 0) {

  def contains(pos: Coordinate) = pos.isInside(width, height)
  def get(pos: Coordinate): Tile = tiles.get(pos).getOrElse(Invalid())
  def isUnderwater(pos: Coordinate, water: Int) = (height - pos.y - 1) <= water
  def isUnderwater = (height - robotPos.y - 1) <= water

  lazy val empty = (for (i <- Range(0, width); j <- Range(0, height)) yield (Coordinate(i, j) -> Empty())).toMap[Coordinate, Tile]
  lazy val sortedKeys = for (i <- Range(0, width); j <- Range(height-1, -1, -1)) yield Coordinate(i, j)

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

  def printStatus() {
    println("-----------")
    println(this.toString)
    println("["+tick+"] Caught " + lambdas + "/" + tLambdas + " lambdas, diving " + isUnderwater + " (" + water + ") for ("+ticksUnderwater+"), hence you('re) " + status)
    println("-----------")
  }

  def iterator = TreeMap(tiles.toArray: _*).iterator

  def applyPatterns(b: Board, opCode: Opcode, patterns: List[Pattern]): Board = {
    val ts = b.sortedKeys flatMap { pos =>
      patterns.filter(_.pred(b, opCode)) collect { case p if (p.isMatch(b, pos)) => (p.replace(b, pos), p.f) }
    }

    val newBoard = ts.foldLeft(b)((acc, t) => b.copy(tiles = acc.tiles ++ t._1))
    ts.foldLeft(newBoard)((acc, t) => t._2(acc))
  }

  def eval(o: Opcode): Board = {
    import Board._

    val newBoardA = applyPatterns(this, o, List(MvRight, MvLeft, MvUp, MvDown, PushRight, PushLeft, MvRightWin, MvLeftWin, MvUpWin, MvDownWin, MvRightEat, MvLeftEat, MvUpEat, MvDownEat))
    val newBoardB = applyPatterns(newBoardA, o, List(Fall, FallRight, FallLeft, FallRightR, openGate))
    val newBoardC = applyPatterns(newBoardB, o, List(Die))

    val newWater = if (flooding != 0 && tick % flooding == 0) newBoardC.water + 1 else newBoardC.water
    val newTicksUnderwater = if (newBoardC.isUnderwater) newBoardC.ticksUnderwater + 1 else 0

    newBoardC.copy(tick = newBoardC.tick + 1,
                   water = newWater,
                   ticksUnderwater = newTicksUnderwater,
                   status = if (newTicksUnderwater > newBoardC.waterProof) Lost() else newBoardC.status)
  }
}

object Board {
  def OpcodePred(o: Opcode) = (_: Board, opCode: Opcode) => o.getClass.isAssignableFrom(opCode.getClass)

  val openGate  = Pattern( { (b, _) => b.tLambdas == b.lambdas },
                          Map((0, 0) -> ClosedLift()),
                          Map((0, 0) -> OpenLift()))

  val PushRight = Pattern(OpcodePred(MoveRight()),
                          Map((0, 0) -> Robot(), (1, 0) -> StableRock(), (2, 0) -> Empty()),
                          Map((0, 0) -> Empty(), (1, 0) -> Robot(), (2, 0) -> StableRock()), { s => s.copy(robotPos = s.robotPos + Coordinate(1, 0)) } )

  val PushLeft  = Pattern(OpcodePred(MoveLeft()),
                          Map((0, 0) -> Empty(), (1, 0) -> StableRock(), (2, 0) -> Robot()),
                          Map((0, 0) -> StableRock(), (1, 0) -> Robot(), (2, 0) -> Empty()), { s => s.copy(robotPos = s.robotPos + Coordinate(-1, 0)) } )

  val Fall      = Pattern(OpcodePred(Opcode()),
                          Map((0, 0) -> Rock(),  (0, 1) -> Empty()),
                          Map((0, 0) -> Empty(), (0, 1) -> FallingRock()))

  val FallRight = Pattern(OpcodePred(Opcode()),
                          Map((0, 0) -> Rock(),  (1, 0) -> Empty(), (0, 1) -> Rock(), (1, 1) -> Empty()),
                          Map((0, 0) -> Empty(), (1, 0) -> Empty(), (0, 1) -> Rock(), (1, 1) -> FallingRock()))

  val FallLeft  = Pattern(OpcodePred(Opcode()),
                          Map((0, 0) -> Empty(), (1, 0) -> Rock(), (0, 1) -> Empty(), (1, 1) -> Rock()),
                          Map((0, 0) -> Empty(), (1, 0) -> Empty(), (0, 1) -> FallingRock(), (1, 1) -> Rock()))

  val FallRightR = Pattern(OpcodePred(Opcode()),
                          Map((0, 0) -> Rock(),  (1, 0) -> Empty(), (0, 1) -> Lambda(), (1, 1) -> Empty()),
                          Map((0, 0) -> Empty(), (1, 0) -> Empty(), (0, 1) -> Lambda(), (1, 1) -> FallingRock()))

  val MvRight   = Pattern(OpcodePred(MoveRight()),
                          Map((0, 0) -> Robot(), (1, 0) -> Reachable()),
                          Map((0, 0) -> Empty(), (1, 0) -> Robot()), { s => s.copy(robotPos = s.robotPos + Coordinate(1, 0)) } )

  val MvLeft    = Pattern(OpcodePred(MoveLeft()),
                          Map((0, 0) -> Reachable(), (1, 0) -> Robot()),
                          Map((0, 0) -> Robot(), (1, 0) -> Empty()), { s => s.copy(robotPos = s.robotPos + Coordinate(-1, 0)) } )

  val MvUp      = Pattern(OpcodePred(MoveUp()),
                          Map((0, 0) -> Reachable(), (0, 1) -> Robot()),
                          Map((0, 0) -> Robot(), (0, 1) -> Empty()), { s => s.copy(robotPos = s.robotPos + Coordinate(0, -1)) } )

  val MvDown    = Pattern(OpcodePred(MoveDown()),
                          Map((0, 0) -> Robot(), (0, 1) -> Reachable()),
                          Map((0, 0) -> Empty(), (0, 1) -> Robot()), { s => s.copy(robotPos = s.robotPos + Coordinate(0, 1)) } )

  val MvRightWin = Pattern(OpcodePred(MoveRight()),
                          Map((0, 0) -> Robot(), (1, 0) -> OpenLift()),
                          Map((0, 0) -> Empty(), (1, 0) -> WinningRobot()), { s => s.copy(robotPos = s.robotPos + Coordinate(1, 0), status = Win()) } )

  val MvLeftWin  = Pattern(OpcodePred(MoveLeft()),
                          Map((0, 0) -> OpenLift(), (1, 0) -> WinningRobot()),
                          Map((0, 0) -> Robot(), (1, 0) -> Empty()), { s => s.copy(robotPos = s.robotPos + Coordinate(-1, 0), status = Win()) } )

  val MvUpWin      = Pattern(OpcodePred(MoveUp()),
                          Map((0, 0) -> OpenLift(), (0, 1) -> WinningRobot()),
                          Map((0, 0) -> Robot(), (0, 1) -> Empty()), { s => s.copy(robotPos = s.robotPos + Coordinate(0, -1), status = Win()) } )

  val MvDownWin    = Pattern(OpcodePred(MoveDown()),
                          Map((0, 0) -> WinningRobot(), (0, 1) -> OpenLift()),
                          Map((0, 0) -> Empty(), (0, 1) -> Robot()), { s => s.copy(robotPos = s.robotPos + Coordinate(0, 1), status = Win()) } )

  val Die       = Pattern(OpcodePred(MoveDown()),
                          Map((0, 0) -> FallingRock(), (0, 1) -> Robot()),
                          Map((0, 0) -> Empty(), (0, 1) -> DeadRobot()), { s => s.copy(status = Lost()) })

  val MvRightEat = Pattern(OpcodePred(MoveRight()),
                          Map((0, 0) -> Robot(), (1, 0) -> Lambda()),
                          Map((0, 0) -> Empty(), (1, 0) -> Robot()), { s => s.copy(lambdas = s.lambdas + 1, robotPos = s.robotPos + Coordinate(1, 0)) } )

  val MvLeftEat  = Pattern(OpcodePred(MoveLeft()),
                          Map((0, 0) -> Lambda(), (1, 0) -> Robot()),
                          Map((0, 0) -> Robot(), (1, 0) -> Empty()), { s => s.copy(lambdas = s.lambdas + 1, robotPos = s.robotPos + Coordinate(-1, 0)) } )

  val MvUpEat      = Pattern(OpcodePred(MoveUp()),
                          Map((0, 0) -> Lambda(), (0, 1) -> Robot()),
                          Map((0, 0) -> Robot(), (0, 1) -> Empty()), { s => s.copy(lambdas = s.lambdas + 1, robotPos = s.robotPos + Coordinate(0, -1)) } )

  val MvDownEat    = Pattern(OpcodePred(MoveDown()),
                          Map((0, 0) -> Robot(), (0, 1) -> Lambda()),
                          Map((0, 0) -> Empty(), (0, 1) -> Robot()), { s => s.copy(lambdas = s.lambdas + 1, robotPos = s.robotPos + Coordinate(0, 1)) } )

  type Metadata = (Int, Int, Int)

  def create(board: Seq[String], metadata: Metadata = (-1, 0, 10)): Board = {
    val width = board.head.length
    val height = board.length

    val (water, flooding, waterproof) = metadata

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

    new Board(width, height, tiles.toMap, robotPos, water, flooding, waterproof, tLambdas = tiles.count { case (_, x) ⇒ x.isInstanceOf[Lambda] })
  }

  def apply(): Board = {
    val board = create(Source.stdin.getLines().takeWhile(_ != "").toSeq)
    val metadata = Source.stdin.getLines().takeWhile(_ != "")

    board
  }
}
