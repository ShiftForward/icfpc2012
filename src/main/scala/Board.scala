import collection.mutable
import scala.io.Source
import collection.immutable.TreeMap
import Tile._
import Opcode._
import Coordinate.Implicits._

final case class Pattern(pred: (Board, Opcode) => Boolean, source: Seq[((Int, Int), Tile)], dest: Seq[((Int, Int), Tile)], f: (Board => Board) = identity) {
  def isMatch(b: Board, pos: Coordinate) =
    source forall { case ((x, y), t) => <~(b.get(Coordinate(x + pos.x, y + pos.y)), t) }

  def replace(b: Board, pos: Coordinate): Seq[(Coordinate, Tile)] =
    dest.map(hp => Coordinate(hp._1._1 + pos.x, hp._1._2 + pos.y) -> hp._2)
}

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

  @inline def OpcodePred(o: Opcode) = (_: Board, opCode: Opcode) => <~~(opCode, o)

  var lastGen: IndexedSeq[Coordinate] = null
  def sortedKeys(width: Int, height: Int): IndexedSeq[Coordinate] = {
    if (lastGen == null) lastGen = for (i <- Range(0, width); j <- Range(height-1, -1, -1)) yield Coordinate(i, j)
    lastGen
  }

  // Push Rocks
  val PushRight   = Pattern(OpcodePred('MoveRight),
                          Seq((0, 0) -> 'Robot, (1, 0) -> 'Rock,  (2, 0) -> 'Empty),
                          Seq((0, 0) -> 'Empty, (1, 0) -> 'Robot, (2, 0) -> 'Rock),      { s => s.copy(robotPos = s.robotPos + Coordinate(1, 0)) } )

  val PushRightHO = Pattern(OpcodePred('MoveRight),
                          Seq((0, 0) -> 'Robot, (1, 0) -> 'HORock, (2, 0) -> 'Empty),
                          Seq((0, 0) -> 'Empty, (1, 0) -> 'Robot,  (2, 0) -> 'HORock),   { s => s.copy(robotPos = s.robotPos + Coordinate(1, 0)) } )

  val PushLeft    = Pattern(OpcodePred('MoveLeft),
                          Seq((-2, 0) -> 'Empty, (-1, 0) -> 'Rock,  (0, 0) -> 'Robot),
                          Seq((-2, 0) -> 'Rock,  (-1, 0) -> 'Robot, (0, 0) -> 'Empty),   { s => s.copy(robotPos = s.robotPos + Coordinate(-1, 0)) } )

  val PushLeftHO  = Pattern(OpcodePred('MoveLeft),
                          Seq((-2, 0) -> 'Empty,  (-1, 0) -> 'HORock, (0, 0) -> 'Robot),
                          Seq((-2, 0) -> 'HORock, (-1, 0) -> 'Robot,  (0, 0) -> 'Empty), { s => s.copy(robotPos = s.robotPos + Coordinate(-1, 0)) } )

  // --- The Falling of a Rock
  val Fall      = Pattern((_, _) => true, Seq((0, 0) -> 'Rock,   (0, 1) -> 'Empty), Seq((0, 0) -> 'Empty, (0, 1) -> 'FallingRock))
  val FallHO    = Pattern((_, _) => true, Seq((0, 0) -> 'HORock, (0, 1) -> 'Empty), Seq((0, 0) -> 'Empty, (0, 1) -> 'HOFallingRock))

  val FallRight  = Pattern((_, _) => true, Seq(( 0, 0) -> 'Rock,  (1, 0) -> 'Empty, ( 0, 1) -> 'Rock,  (1, 1) -> 'Empty), Seq((0, 0) -> 'Empty, ( 1, 1) -> 'FallingRock))
  val FallLeft   = Pattern((_, _) => true, Seq((-1, 0) -> 'Empty, (0, 0) -> 'Rock,  (-1, 1) -> 'Empty, (0, 1) -> 'Rock),  Seq((0, 0) -> 'Empty, (-1, 1) -> 'FallingRock))
  val FallRightR = Pattern((_, _) => true, Seq(( 0, 0) -> 'Rock,  (1, 0) -> 'Empty, (0, 1) -> 'Lambda, (1, 1) -> 'Empty), Seq((0, 0) -> 'Empty, ( 1, 1) -> 'FallingRock))

  val FallRightHO  = Pattern((_, _) => true, Seq(( 0, 0) -> 'HORock, (1, 0) -> 'Empty,  ( 0, 1) -> 'Rock,  (1, 1) -> 'Empty), Seq((0, 0) -> 'Empty, ( 1, 1) -> 'HOFallingRock))
  val FallLeftHO   = Pattern((_, _) => true, Seq((-1, 0) -> 'Empty,  (0, 0) -> 'HORock, (-1, 1) -> 'Empty, (0, 1) -> 'Rock),  Seq((0, 0) -> 'Empty, (-1, 1) -> 'HOFallingRock))
  val FallRightRHO = Pattern((_, _) => true, Seq(( 0, 0) -> 'HORock, (1, 0) -> 'Empty,  (0, 1) -> 'Lambda, (1, 1) -> 'Empty), Seq((0, 0) -> 'Empty, ( 1, 1) -> 'HOFallingRock))

  // --- Stabilization of Rocks when falling
  val StableW    = Pattern((_, _) => true, Seq((0, 0) -> 'FallingRock,   (0, 1) -> 'Wall),       Seq((0, 0) -> 'Rock))
  val StableR    = Pattern((_, _) => true, Seq((0, 0) -> 'FallingRock,   (0, 1) -> 'Rock),       Seq((0, 0) -> 'Rock))
  val StableL    = Pattern((_, _) => true, Seq((0, 0) -> 'FallingRock,   (0, 1) -> 'Lambda),     Seq((0, 0) -> 'Rock))
  val StableOl   = Pattern((_, _) => true, Seq((0, 0) -> 'FallingRock,   (0, 1) -> 'OpenLift),   Seq((0, 0) -> 'Rock))
  val StableCl   = Pattern((_, _) => true, Seq((0, 0) -> 'FallingRock,   (0, 1) -> 'ClosedLift), Seq((0, 0) -> 'Rock))
  val StableE    = Pattern((_, _) => true, Seq((0, 0) -> 'FallingRock,   (0, 1) -> 'Earth),      Seq((0, 0) -> 'Rock))
  val StableWHO  = Pattern((_, _) => true, Seq((0, 0) -> 'HOFallingRock, (0, 1) -> 'Wall),       Seq((0, 0) -> 'Lambda))
  val StableRHO  = Pattern((_, _) => true, Seq((0, 0) -> 'HOFallingRock, (0, 1) -> 'Rock),       Seq((0, 0) -> 'Lambda))
  val StableLHO  = Pattern((_, _) => true, Seq((0, 0) -> 'HOFallingRock, (0, 1) -> 'Lambda),     Seq((0, 0) -> 'Lambda))
  val StableOlHO = Pattern((_, _) => true, Seq((0, 0) -> 'HOFallingRock, (0, 1) -> 'OpenLift),   Seq((0, 0) -> 'Lambda))
  val StableClHO = Pattern((_, _) => true, Seq((0, 0) -> 'HOFallingRock, (0, 1) -> 'ClosedLift), Seq((0, 0) -> 'Lambda))
  val StableEHO  = Pattern((_, _) => true, Seq((0, 0) -> 'HOFallingRock, (0, 1) -> 'Earth),      Seq((0, 0) -> 'Lambda))

  // --- Losing Conditions
  val Die         = Pattern((_, _) => true, Seq((0, -1) -> 'FallingRock,   (0, 0) -> 'Robot), Seq(), { s => s.copy(status = 'Lost) })
  val DieHO       = Pattern((_, _) => true, Seq((0, -1) -> 'HOFallingRock, (0, 0) -> 'Robot), Seq(), { s => s.copy(status = 'Lost) })
  val DieOutrun   = Pattern(OpcodePred('MoveDown), Seq((0, -1) -> 'Rock,   (0, 0) -> 'Robot), Seq(), { s => s.copy(status = 'Lost) })
  val DieOutrunHO = Pattern(OpcodePred('MoveDown), Seq((0, -1) -> 'HORock, (0, 0) -> 'Robot), Seq(), { s => s.copy(status = 'Lost) })

  // --- Winning Conditions
  val openGate  = Pattern( { (b, _) => b.tLambdas == b.lambdas }, Seq((0, 0) -> 'ClosedLift), Seq((0, 0) -> 'OpenLift))

  // --- Movements
  val MvRight    = Pattern(OpcodePred('MoveRight),
                          Seq((0, 0) -> 'Robot, (1, 0) -> 'Reachable),
                          Seq((0, 0) -> 'Empty, (1, 0) -> 'Robot), { s => s.copy(robotPos = s.robotPos + Coordinate(1, 0)) } )

  val MvLeft    = Pattern(OpcodePred('MoveLeft),
                          Seq((-1, 0) -> 'Reachable, (0, 0) -> 'Robot),
                          Seq((-1, 0) -> 'Robot, (0, 0) -> 'Empty), { s => s.copy(robotPos = s.robotPos + Coordinate(-1, 0)) } )

  val MvUp      = Pattern(OpcodePred('MoveUp),
                          Seq((0, -1) -> 'Reachable, (0, 0) -> 'Robot),
                          Seq((0, -1) -> 'Robot, (0, 0) -> 'Empty), { s => s.copy(robotPos = s.robotPos + Coordinate(0, -1)) } )

  val MvDown    = Pattern(OpcodePred('MoveDown),
                          Seq((0, 0) -> 'Robot, (0, 1) -> 'Reachable),
                          Seq((0, 0) -> 'Empty, (0, 1) -> 'Robot), { s => s.copy(robotPos = s.robotPos + Coordinate(0, 1)) } )

  val MvRightWin = Pattern(OpcodePred('MoveRight),
                          Seq((0, 0) -> 'Robot, (1, 0) -> 'OpenLift),
                          Seq((0, 0) -> 'Empty, (1, 0) -> 'Robot), { s => s.copy(robotPos = s.robotPos + Coordinate(1, 0), status = 'Win) } )

  val MvLeftWin  = Pattern(OpcodePred('MoveLeft),
                          Seq((-1, 0) -> 'OpenLift, (0, 0) -> 'Robot),
                          Seq((-1, 0) -> 'Robot, (0, 0) -> 'Empty), { s => s.copy(robotPos = s.robotPos + Coordinate(-1, 0), status = 'Win) } )

  val MvUpWin      = Pattern(OpcodePred('MoveUp),
                          Seq((0, -1) -> 'OpenLift, (0, 0) -> 'Robot),
                          Seq((0, -1) -> 'Robot, (0, 0) -> 'Empty), { s => s.copy(robotPos = s.robotPos + Coordinate(0, -1), status = 'Win) } )

  val MvDownWin    = Pattern(OpcodePred('MoveDown),
                          Seq((0, 0) -> 'Robot, (0, 1) -> 'OpenLift),
                          Seq((0, 0) -> 'Empty, (0, 1) -> 'Robot), { s => s.copy(robotPos = s.robotPos + Coordinate(0, 1), status = 'Win) } )

  val MvRightEat = Pattern(OpcodePred('MoveRight),
                          Seq((0, 0) -> 'Robot, (1, 0) -> 'Lambda),
                          Seq((0, 0) -> 'Empty, (1, 0) -> 'Robot), { s => s.copy(lambdas = s.lambdas + 1, robotPos = s.robotPos + Coordinate(1, 0)) } )

  val MvLeftEat  = Pattern(OpcodePred('MoveLeft),
                          Seq((-1, 0) -> 'Lambda, (0, 0) -> 'Robot),
                          Seq((-1, 0) -> 'Robot, (0, 0) -> 'Empty), { s => s.copy(lambdas = s.lambdas + 1, robotPos = s.robotPos + Coordinate(-1, 0)) } )

  val MvUpEat      = Pattern(OpcodePred('MoveUp),
                          Seq((0, -1) -> 'Lambda, (0, 0) -> 'Robot),
                          Seq((0, -1) -> 'Robot, (0, 0) -> 'Empty), { s => s.copy(lambdas = s.lambdas + 1, robotPos = s.robotPos + Coordinate(0, -1)) } )

  val MvDownEat    = Pattern(OpcodePred('MoveDown),
                          Seq((0, 0) -> 'Robot, (0, 1) -> 'Lambda),
                          Seq((0, 0) -> 'Empty, (0, 1) -> 'Robot), { s => s.copy(lambdas = s.lambdas + 1, robotPos = s.robotPos + Coordinate(0, 1)) } )

  val MvRightRazor = Pattern(OpcodePred('MoveRight),
                          Seq((0, 0) -> 'Robot, (1, 0) -> 'Razor),
                          Seq((0, 0) -> 'Empty, (1, 0) -> 'Robot), { s => s.copy(nRazors = s.nRazors + 1, robotPos = s.robotPos + Coordinate(1, 0)) } )

  val MvLeftRazor  = Pattern(OpcodePred('MoveLeft),
                          Seq((-1, 0) -> 'Razor, (0, 0) -> 'Robot),
                          Seq((-1, 0) -> 'Robot, (0, 0) -> 'Empty), { s => s.copy(nRazors = s.nRazors + 1, robotPos = s.robotPos + Coordinate(-1, 0)) } )

  val MvUpRazor      = Pattern(OpcodePred('MoveUp),
                          Seq((0, -1) -> 'Razor, (0, 0) -> 'Robot),
                          Seq((0, -1) -> 'Robot, (0, 0) -> 'Empty), { s => s.copy(nRazors = s.nRazors + 1, robotPos = s.robotPos + Coordinate(0, -1)) } )

  val MvDownRazor    = Pattern(OpcodePred('MoveDown),
                          Seq((0, 0) -> 'Robot, (0, 1) -> 'Razor),
                          Seq((0, 0) -> 'Empty, (0, 1) -> 'Robot), { s => s.copy(nRazors = s.nRazors + 1, robotPos = s.robotPos + Coordinate(0, 1)) } )

  // --- Trampolines

  def trampF(c: Coordinate)(s: Board) = {
    val newPos  = s.trampolines(s.robotPos + c)
    val sources = s.trampolines.filter(_._2 == newPos).map(_._1 -> 'Empty)
    s.copy(robotPos = newPos, tiles = (s.tiles + (newPos -> 'Robot)) ++ sources)
  }

  val MvRightT       = Pattern(OpcodePred('MoveRight),
                          Seq((0, 0) -> 'Robot, (1, 0) -> 'Trampoline),
                          Seq((0, 0) -> 'Empty, (1, 0) -> 'Empty), trampF(Coordinate(1, 0)))

  val MvLeftT        = Pattern(OpcodePred('MoveLeft),
                          Seq((-1, 0) -> 'Trampoline, (0, 0) -> 'Robot),
                          Seq((-1, 0) -> 'Empty,      (0, 0) -> 'Empty), trampF(Coordinate(-1, 0)))

  val MvUpT          = Pattern(OpcodePred('MoveUp),
                          Seq((0, -1) -> 'Trampoline, (0, 0) -> 'Robot),
                          Seq((0, -1) -> 'Empty,      (0, 0) -> 'Empty), trampF(Coordinate(-1, 0)))

  val MvDownT        = Pattern(OpcodePred('MoveDown),
                          Seq((0, 0) -> 'Robot, (0, 1) -> 'Trampoline),
                          Seq((0, 0) -> 'Empty, (0, 1) -> 'Empty), trampF(Coordinate(1, 0)))

  val beardGrowthPred = (b: Board, _: Opcode) => (b.tick % b.growth == (b.growth -1))

  val BeardGrowthN  = Pattern(beardGrowthPred, Seq((0, 0) -> 'Beard, ( 0, -1) -> 'Empty), Seq(( 0, -1) -> 'Beard))
  val BeardGrowthNE = Pattern(beardGrowthPred, Seq((0, 0) -> 'Beard, ( 1, -1) -> 'Empty), Seq(( 1, -1) -> 'Beard))
  val BeardGrowthE  = Pattern(beardGrowthPred, Seq((0, 0) -> 'Beard, ( 1,  0) -> 'Empty), Seq(( 1,  0) -> 'Beard))
  val BeardGrowthSE = Pattern(beardGrowthPred, Seq((0, 0) -> 'Beard, ( 1,  1) -> 'Empty), Seq(( 1,  1) -> 'Beard))
  val BeardGrowthS  = Pattern(beardGrowthPred, Seq((0, 0) -> 'Beard, ( 0,  1) -> 'Empty), Seq(( 0,  1) -> 'Beard))
  val BeardGrowthSW = Pattern(beardGrowthPred, Seq((0, 0) -> 'Beard, (-1, -1) -> 'Empty), Seq((-1, -1) -> 'Beard))
  val BeardGrowthW  = Pattern(beardGrowthPred, Seq((0, 0) -> 'Beard, (-1,  0) -> 'Empty), Seq((-1,  0) -> 'Beard))
  val BeardGrowthNW = Pattern(beardGrowthPred, Seq((0, 0) -> 'Beard, (-1,  1) -> 'Empty), Seq((-1,  1) -> 'Beard))

  val RazorBlowN  = Pattern(OpcodePred('Razor), Seq((0, 0) -> 'Robot, ( 0, -1) -> 'Beard), Seq(( 0, -1) -> 'Empty))
  val RazorBlowNE = Pattern(OpcodePred('Razor), Seq((0, 0) -> 'Robot, ( 1, -1) -> 'Beard), Seq(( 1, -1) -> 'Empty))
  val RazorBlowE  = Pattern(OpcodePred('Razor), Seq((0, 0) -> 'Robot, ( 1,  0) -> 'Beard), Seq(( 1,  0) -> 'Empty))
  val RazorBlowSE = Pattern(OpcodePred('Razor), Seq((0, 0) -> 'Robot, ( 1,  1) -> 'Beard), Seq(( 1,  1) -> 'Empty))
  val RazorBlowS  = Pattern(OpcodePred('Razor), Seq((0, 0) -> 'Robot, ( 0,  1) -> 'Beard), Seq(( 0,  1) -> 'Empty))
  val RazorBlowSW = Pattern(OpcodePred('Razor), Seq((0, 0) -> 'Robot, (-1, -1) -> 'Beard), Seq((-1, -1) -> 'Empty))
  val RazorBlowW  = Pattern(OpcodePred('Razor), Seq((0, 0) -> 'Robot, (-1,  0) -> 'Beard), Seq((-1,  0) -> 'Empty))
  val RazorBlowNW = Pattern(OpcodePred('Razor), Seq((0, 0) -> 'Robot, (-1,  1) -> 'Beard), Seq((-1,  1) -> 'Empty))
  val UseRazor    = Pattern(OpcodePred('Razor), Seq(), Seq(), { s => s.copy(nRazors = s.nRazors - 1) })

  val stabilization =
    List(StableW, StableR, StableL, StableOl, StableCl, StableE, StableWHO, StableRHO, StableLHO, StableOlHO, StableClHO, StableEHO)

  val moves =
    List(MvRight, MvLeft, MvUp, MvDown, PushRight, PushLeft, MvRightWin, MvLeftWin, MvUpWin, MvDownWin,
         MvRightEat, MvLeftEat, MvUpEat, MvDownEat,
         MvRightRazor, MvLeftRazor, MvUpRazor, MvDownRazor,
         MvRightT, MvLeftT, MvUpT, MvDownT)

  val fallsAndBeards =
    List(Fall, FallHO, FallRight, FallRightHO, FallRightR, FallRightRHO, openGate,
         BeardGrowthN, BeardGrowthNE, BeardGrowthE, BeardGrowthSE, BeardGrowthS, BeardGrowthSW, BeardGrowthW, BeardGrowthNW)

  val fallLefts = List(FallLeft, FallLeftHO)

  val razors =
    List(RazorBlowN, RazorBlowNE, RazorBlowE, RazorBlowSE, RazorBlowS, RazorBlowSW, RazorBlowW, RazorBlowNW, Die, DieOutrun, DieHO, DieOutrunHO, UseRazor)

  val patterns = List((stabilization, { (b: Board, pos: Coordinate) =>
                        val o = b.get(pos); o == 'FallingRock || o == 'HOFallingRock }),

                      (moves, { (b: Board, pos: Coordinate) =>
                        b.get(pos) == 'Robot }),

                      (fallsAndBeards, { (b: Board, pos: Coordinate) =>
                        val o = b.get(pos); <~(o, 'Rock) || <~(o, 'HORock) || o == 'ClosedLift || o == 'Beard }),

                      (fallLefts, { (b: Board, pos: Coordinate) =>
                        val o = b.get(pos); <~(o, 'Rock) || <~(o, 'HORock) }),

                      (razors, { (b: Board, pos: Coordinate) =>
                        b.get(pos) == 'Robot }))

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
