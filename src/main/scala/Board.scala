import scala.io.Source
import collection.immutable.{HashMap, TreeMap}
import Tile._
import Opcode._
import Coordinate.Implicits._

final case class Pattern(pred: (Board, Opcode) => Boolean, source: Map[(Int, Int), Tile], dest: Map[(Int, Int), Tile], f: (Board => Board) = identity) {
  def isMatch(b: Board, pos: Coordinate) =
    source.forall(hp => <~(b.get(Coordinate(hp._1._1 + pos.x, hp._1._2 + pos.y)), hp._2))

  def replace(b: Board, pos: Coordinate): Map[Coordinate, Tile] =
    dest.map(hp => Coordinate(hp._1._1 + pos.x, hp._1._2 + pos.y) -> hp._2)
}

case class Board(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate, water: Int = -1, flooding: Int = 0,
                 waterProof: Int = 10, tick: Int = 0, lambdas: Int = 0, status: Status = Playing(), tLambdas: Int = 0, ticksUnderwater: Int = 0) {

  @inline def get(pos: Coordinate): Tile = tiles.get(pos).getOrElse('Invalid)
  @inline def isUnderwater = (height - robotPos.y - 1) <= water

  override def toString = {
    val lines = TreeMap(tiles.toArray: _*).groupBy { case (pos, _) => pos.y }
    val sortedLines = TreeMap(lines.toArray: _*)

    sortedLines.map { case (n, line) =>
      line.map { case (_, tile) =>
        tile match {
          case 'Robot => 'R'
          case 'Wall => '#'
          case 'Lambda => '\\'
          case 'Earth => '.'
          case 'Empty => ' '
          case 'ClosedLift => 'L'
          case 'OpenLift => 'O'
          case 'Rock | 'FallingRock | 'StableRock => '*'
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

  def allLambdas = {
    tiles.foldLeft(List[Coordinate]()) { case (list, (coordinate, tile)) =>
      tile match {
        case 'Lambda => coordinate :: list
        case _ => list
      }
    }.sortBy { _.distance(robotPos) }
  }

  def liftPosition = tiles.find { _._2 == 'OpenLift }.map(_._1).get

  def getClosest(c: Coordinate, t: Tile, l: Int = 20): List[Coordinate] = {
    val coordinates = for (i <- (-l/2 to l/2); j <- (-l/2 to l/2)) yield Coordinate(i, j)

    coordinates.foldLeft(List[Coordinate]()) { (l, cc) =>
      val currentC = c + cc
      if (<~(get(currentC), t)) currentC :: l else l
    }
  }

  def getClosest(t: Tile): List[Coordinate] = getClosest(robotPos, t)

  def applyPatterns(b: Board, opCode: Opcode, patterns: List[Pattern], keys: IndexedSeq[Coordinate]): Board = {
    val ps = patterns.filter(_.pred(b, opCode))
    val ts = keys filterNot { pos => val o = b.get(pos); o == 'Empty || o == 'Lambda || o == 'Earth || o == 'Wall } flatMap { pos =>
       ps collect { case p if (p.isMatch(b, pos)) => (p.replace(b, pos), p.f) }
    }

    val newBoard = b.copy(tiles = ts.foldLeft(b.tiles)((acc, t) => acc ++ t._1))
    ts.foldLeft(newBoard)((acc, t) => t._2(acc))
  }

  def eval(lo: List[Opcode]): Board = {
    lo.foldLeft(this) { (b, o) => b.eval(o) }
  }

  def eval(o: Opcode): Board = {
    import Board._

    val keys = Board.sortedKeys(this.width, this.height)
    val newBoardA = applyPatterns(this, o, tier1, keys)
    val newBoardB = applyPatterns(newBoardA, o, tier2, keys)
    val newBoardC = applyPatterns(newBoardB, o, tier3, keys)

    val newWater = if (flooding != 0 && tick % flooding == 0) newBoardC.water + 1 else newBoardC.water
    val newTicksUnderwater = if (newBoardC.isUnderwater) newBoardC.ticksUnderwater + 1 else 0

    newBoardC.copy(tick = newBoardC.tick + 1,
                   water = newWater,
                   ticksUnderwater = newTicksUnderwater,
                   status = if (newTicksUnderwater > newBoardC.waterProof) Lost() else newBoardC.status)
  }
}

object Board {
  @inline def OpcodePred(o: Opcode) = (_: Board, opCode: Opcode) => <~~(opCode, o)

  var lastGen: IndexedSeq[Coordinate] = null
  def sortedKeys(width: Int, height: Int): IndexedSeq[Coordinate] = {
    if (lastGen == null) lastGen = for (i <- Range(0, width); j <- Range(height-1, -1, -1)) yield Coordinate(i, j)
    lastGen
  }

  val openGate  = Pattern( { (b, _) => b.tLambdas == b.lambdas },
                          Map((0, 0) -> 'ClosedLift),
                          Map((0, 0) -> 'OpenLift))

  val PushRight = Pattern(OpcodePred('MoveRight),
                          Map((0, 0) -> 'Robot, (1, 0) -> 'StableRock, (2, 0) -> 'Empty),
                          Map((0, 0) -> 'Empty, (1, 0) -> 'Robot, (2, 0) -> 'StableRock), { s => s.copy(robotPos = s.robotPos + Coordinate(1, 0)) } )

  val PushLeft  = Pattern(OpcodePred('MoveLeft),
                          Map((-2, 0) -> 'Empty, (-1, 0) -> 'StableRock, (0, 0) -> 'Robot),
                          Map((-2, 0) -> 'StableRock, (-1, 0) -> 'Robot, (0, 0) -> 'Empty), { s => s.copy(robotPos = s.robotPos + Coordinate(-1, 0)) } )

  val Fall      = Pattern((_, _) => true,
                          Map((0, 0) -> 'Rock,  (0, 1) -> 'Empty),
                          Map((0, 0) -> 'Empty, (0, 1) -> 'FallingRock))

  val FallRight = Pattern((_, _) => true,
                          Map((0, 0) -> 'Rock,  (1, 0) -> 'Empty, (0, 1) -> 'Rock, (1, 1) -> 'Empty),
                          Map((0, 0) -> 'Empty, (1, 0) -> 'Empty, (0, 1) -> 'Rock, (1, 1) -> 'FallingRock))

  val FallLeft  = Pattern((_, _) => true,
                          Map((-1, 0) -> 'Empty, (0, 0) -> 'Rock, (-1, 1) -> 'Empty, (0, 1) -> 'Rock),
                          Map((-1, 0) -> 'Empty, (0, 0) -> 'Empty, (-1, 1) -> 'FallingRock, (0, 1) -> 'Rock))

  val FallRightR = Pattern((_, _) => true,
                          Map((0, 0) -> 'Rock,  (1, 0) -> 'Empty, (0, 1) -> 'Lambda, (1, 1) -> 'Empty),
                          Map((0, 0) -> 'Empty, (1, 0) -> 'Empty, (0, 1) -> 'Lambda, (1, 1) -> 'FallingRock))

  val MvRight   = Pattern(OpcodePred('MoveRight),
                          Map((0, 0) -> 'Robot, (1, 0) -> 'Reachable),
                          Map((0, 0) -> 'Empty, (1, 0) -> 'Robot), { s => s.copy(robotPos = s.robotPos + Coordinate(1, 0)) } )

  val MvLeft    = Pattern(OpcodePred('MoveLeft),
                          Map((-1, 0) -> 'Reachable, (0, 0) -> 'Robot),
                          Map((-1, 0) -> 'Robot, (0, 0) -> 'Empty), { s => s.copy(robotPos = s.robotPos + Coordinate(-1, 0)) } )

  val MvUp      = Pattern(OpcodePred('MoveUp),
                          Map((0, -1) -> 'Reachable, (0, 0) -> 'Robot),
                          Map((0, -1) -> 'Robot, (0, 0) -> 'Empty), { s => s.copy(robotPos = s.robotPos + Coordinate(0, -1)) } )

  val MvDown    = Pattern(OpcodePred('MoveDown),
                          Map((0, 0) -> 'Robot, (0, 1) -> 'Reachable),
                          Map((0, 0) -> 'Empty, (0, 1) -> 'Robot), { s => s.copy(robotPos = s.robotPos + Coordinate(0, 1)) } )

  val MvRightWin = Pattern(OpcodePred('MoveRight),
                          Map((0, 0) -> 'Robot, (1, 0) -> 'OpenLift),
                          Map((0, 0) -> 'Empty, (1, 0) -> 'Robot), { s => s.copy(robotPos = s.robotPos + Coordinate(1, 0), status = Win()) } )

  val MvLeftWin  = Pattern(OpcodePred('MoveLeft),
                          Map((-1, 0) -> 'OpenLift, (0, 0) -> 'Robot),
                          Map((-1, 0) -> 'Robot, (0, 0) -> 'Empty), { s => s.copy(robotPos = s.robotPos + Coordinate(-1, 0), status = Win()) } )

  val MvUpWin      = Pattern(OpcodePred('MoveUp),
                          Map((0, -1) -> 'OpenLift, (0, 0) -> 'Robot),
                          Map((0, -1) -> 'Robot, (0, 0) -> 'Empty), { s => s.copy(robotPos = s.robotPos + Coordinate(0, -1), status = Win()) } )

  val MvDownWin    = Pattern(OpcodePred('MoveDown),
                          Map((0, 0) -> 'Robot, (0, 1) -> 'OpenLift),
                          Map((0, 0) -> 'Empty, (0, 1) -> 'Robot), { s => s.copy(robotPos = s.robotPos + Coordinate(0, 1), status = Win()) } )

  val Die       = Pattern((_, _) => true,
                          Map((0, -1) -> 'FallingRock, (0, 0) -> 'Robot),
                          Map((0, -1) -> 'FallingRock, (0, 0) -> 'Robot), { s => s.copy(status = Lost()) })

  val DieOutrun = Pattern(OpcodePred('MoveDown),
                          Map((0, -1) -> 'Rock, (0, 0) -> 'Robot),
                          Map((0, -1) -> 'FallingRock, (0, 0) -> 'Robot), { s => s.copy(status = Lost()) })

  val MvRightEat = Pattern(OpcodePred('MoveRight),
                          Map((0, 0) -> 'Robot, (1, 0) -> 'Lambda),
                          Map((0, 0) -> 'Empty, (1, 0) -> 'Robot), { s => s.copy(lambdas = s.lambdas + 1, robotPos = s.robotPos + Coordinate(1, 0)) } )

  val MvLeftEat  = Pattern(OpcodePred('MoveLeft),
                          Map((-1, 0) -> 'Lambda, (0, 0) -> 'Robot),
                          Map((-1, 0) -> 'Robot, (0, 0) -> 'Empty), { s => s.copy(lambdas = s.lambdas + 1, robotPos = s.robotPos + Coordinate(-1, 0)) } )

  val MvUpEat      = Pattern(OpcodePred('MoveUp),
                          Map((0, -1) -> 'Lambda, (0, 0) -> 'Robot),
                          Map((0, -1) -> 'Robot, (0, 0) -> 'Empty), { s => s.copy(lambdas = s.lambdas + 1, robotPos = s.robotPos + Coordinate(0, -1)) } )

  val MvDownEat    = Pattern(OpcodePred('MoveDown),
                          Map((0, 0) -> 'Robot, (0, 1) -> 'Lambda),
                          Map((0, 0) -> 'Empty, (0, 1) -> 'Robot), { s => s.copy(lambdas = s.lambdas + 1, robotPos = s.robotPos + Coordinate(0, 1)) } )

  val tier1 = List(MvRight, MvLeft, MvUp, MvDown, PushRight, PushLeft, MvRightWin, MvLeftWin, MvUpWin, MvDownWin, MvRightEat, MvLeftEat, MvUpEat, MvDownEat)
  val tier2 = List(Fall, FallRight, FallLeft, FallRightR, openGate)
  val tier3 = List(Die, DieOutrun)

  type Metadata = (Int, Int, Int)

  def create(board: Seq[String], metadata: Metadata = (-1, 0, 10)): Board = {
    val width = board.maxBy(_.length).length
    val height = board.length

    val (water, flooding, waterproof) = metadata

    val tiles = board.zipWithIndex.map { case (line, y) =>
      val paddedLine = line.padTo[Char, String](width, ' ')

      paddedLine.zipWithIndex.map { case (char, x) =>
        ((x, y): Coordinate, Tile(char))
      }
    }.flatten

    val robotPos = tiles.find { _._2 == 'Robot }.map(_._1).get
    new Board(width, height, tiles.toMap, robotPos, water, flooding, waterproof, tLambdas = tiles.count { _._2 == 'Lambda })
  }

  def apply(filename: String): Board = {
    val input = Source.fromFile(filename).getLines().toSeq
    parse(input)
  }

  def apply(): Board = {
    val input = Source.stdin.getLines().toSeq
    parse(input)
  }

  private def parse(input: Seq[String]): Board = {
    val MetadataRegex = """Water (\d+)Flooding (\d+)Waterproof (\d+)""".r

    try {
      val (board, metadata) = input.splitAt(input.length - 3)

      val MetadataRegex(water, flooding, waterproof) = metadata.mkString

      create(board, (water.toInt - 1, flooding.toInt, waterproof.toInt))

    } catch {
      case e: MatchError => create(input)
    }
  }
}
