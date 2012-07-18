import collection.mutable
import scala.io.Source
import collection.immutable.TreeMap
import Tile._
import Opcode._
import Coordinate.Implicits._

case class Board(width: Int, height: Int, tiles: Map[Coordinate, Tile], robotPos: Coordinate, water: Int = -1, flooding: Int = 0,
                 waterProof: Int = 10, tick: Int = 0, lambdas: Int = 0, status: Symbol = 'Playing, tLambdas: Int = 0,
                 ticksUnderwater: Int = 0, nRazors: Int = 0, growth: Int = 25, trampolines: Map[Coordinate, Coordinate]) {

  @inline def get(pos: Coordinate): Tile = tiles.get(pos).getOrElse('Invalid)
  @inline def isUnderwater = (height - robotPos.y - 1) <= water

  override def toString = {
    val lines = TreeMap(tiles.toArray: _*).groupBy(_._1.y)
    val sortedLines = TreeMap(lines.toArray: _*)

    sortedLines.map { _._2.map { _._2 match {
      case 'Robot                   => 'R'
      case 'Wall                    => '#'
      case 'Lambda                  => '\\'
      case 'Earth                   => '.'
      case 'Empty                   => ' '
      case 'Beard                   => 'W'
      case 'ClosedLift              => 'L'
      case 'OpenLift                => 'O'
      case 'Rock | 'FallingRock     => '*'
      case 'HORock | 'HOFallingRock => '@'
      case 'Razor                   => '!'
      case 'TrampolineA             => 'A'
      case 'TrampolineB             => 'B'
      case 'TrampolineC             => 'C'
      case 'TrampolineD             => 'D'
      case 'TrampolineE             => 'E'
      case 'TrampolineF             => 'F'
      case 'TrampolineG             => 'G'
      case 'TrampolineH             => 'H'
      case 'TrampolineI             => 'I'
      case 'Target1                 => '1'
      case 'Target2                 => '2'
      case 'Target3                 => '3'
      case 'Target4                 => '4'
      case 'Target5                 => '5'
      case 'Target6                 => '6'
      case 'Target7                 => '7'
      case 'Target8                 => '8'
      case 'Target9                 => '9'
      case _                        => '?'
    } }.mkString }.mkString("\n")
  }

  def printStatus() {
    println("-----------")
    println(this.toString)
    println("Tick: " + tick)
    println("Caught: " + lambdas + "/" + tLambdas)
    println("Diving: " + isUnderwater + (if (isUnderwater) " (for " + ticksUnderwater + "ticks)" else ""))
    println("Waterproof: " + waterProof)
    println("Razors: " + nRazors)
    println("Beard growth: " + growth)
    println("Status: " + status)
    println("Trampolines " + trampolines)
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

  def applyPatterns(b: Board, patterns: List[Pattern], keys: IndexedSeq[Coordinate])(implicit o: Opcode): Board = {
    val ps = patterns.filter(_.pred(b, o))
    val ts = keys flatMap { pos => ps collect { case p if (p.isMatch(b, pos)) => (p.replace(b, pos), p.f) } }

    val newBoard = b.copy(tiles = ts.foldLeft(b.tiles)((acc, t) => acc ++ t._1))
    ts.foldLeft(newBoard)((acc, t) => t._2(acc))
  }

  def eval(lo: List[Opcode]): Board = {
    lo.foldLeft(this) { (b, o) => b.eval(o) }
  }

  def eval(implicit o: Opcode): Board = {
    import Board._
    import Pattern._

    val keys = Board.sortedKeys(this.width, this.height)

    val newBoard = patterns.foldLeft(this) { case (board, (pattern, predicate)) =>
      applyPatterns(board, pattern, keys filter predicate.curried(board))
    }

    val newWater = if (flooding != 0 && tick % flooding == 0) newBoard.water + 1 else newBoard.water
    val newTicksUnderwater = if (newBoard.isUnderwater) newBoard.ticksUnderwater + 1 else 0

    newBoard.copy(tick = newBoard.tick + 1,
                  water = newWater,
                  ticksUnderwater = newTicksUnderwater,
                  status = if (newTicksUnderwater > newBoard.waterProof) 'Lost else newBoard.status)
  }
}

object Board {

  var lastGen: IndexedSeq[Coordinate] = null
  def sortedKeys(width: Int, height: Int): IndexedSeq[Coordinate] = {
    if (lastGen == null) lastGen = for (i <- Range(0, width); j <- Range(height-1, -1, -1)) yield Coordinate(i, j)
    lastGen
  }

  type Metadata = (Int, Int, Int, Int, Int)

  def create(board: Seq[String], metadata: Metadata = (-1, 0, 10, 0, 25), portals: Map[String, String]): Board = {
    val width = board.maxBy(_.length).length
    val height = board.length

    val (water, flooding, waterproof, razors, growth) = metadata

    val tiles = board.zipWithIndex.map { case (line, y) =>
      val paddedLine = line.padTo[Char, String](width, ' ')

      paddedLine.zipWithIndex.map { case (char, x) =>
        ((x, y): Coordinate, Tile(char))
      }
    }.flatten

    val robotPos    = tiles.find { _._2 == 'Robot }.map(_._1).get
    val nLambdas    = tiles.count { tile => tile._2 == 'Lambda || tile._2 == 'HORock }
    val trampolines = portals.map(kvp => tiles.find { _._2 == Symbol("Trampoline" + kvp._1) }.map(_._1).get ->
                                         tiles.find { _._2 == Symbol("Target"     + kvp._2) }.map(_._1).get)

    new Board(width, height, tiles.toMap, robotPos, water, flooding, waterproof, tLambdas = nLambdas,
              nRazors = razors, growth = growth, trampolines = trampolines)
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
    var (water, flooding, waterproof, razors, growth) = (-1, 0, 10, 0, 25)
    var board = Seq[String]()
    var portals = mutable.HashMap[String, String]()

    val Water = """Water (\d+)""".r
    val Flooding = """Flooding (\d+)""".r
    val WaterProof = """Waterproof (\d+)""".r
    val Razors = """Razors (\d+)""".r
    val Growth = """Growth (\d+)""".r
    val Trampoline = """Trampoline (.) targets (\d)""".r

    input foreach { _ match {
        case Water(w) => water = (w.toInt - 1)
        case Flooding(f) => flooding = f.toInt
        case WaterProof(wp) => waterproof = wp.toInt
        case Razors(r) => razors = r.toInt
        case Growth(g) => growth = g.toInt
        case Trampoline(s, d) => portals += (s -> d.toString)
        case l: String => board = board :+ l
    }}

    create(board, (water, flooding, waterproof, razors, growth), portals.toMap)
  }
}
