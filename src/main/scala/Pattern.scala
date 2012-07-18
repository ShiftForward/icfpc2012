import Tile._
import Opcode._

case class Pattern(pred: (Board, Opcode) => Boolean, source: Seq[((Int, Int), Tile)],
                   dest: Seq[((Int, Int), Tile)], f: (Board => Board) = identity) {

  def isMatch(b: Board, pos: Coordinate) =
    source forall { case ((x, y), t) => <~(b.get(Coordinate(x + pos.x, y + pos.y)), t) }

  def replace(b: Board, pos: Coordinate): Seq[(Coordinate, Tile)] =
    dest.map(hp => Coordinate(hp._1._1 + pos.x, hp._1._2 + pos.y) -> hp._2)
}

object Pattern {
  @inline def OpcodePred(o: Opcode) = (_: Board, opCode: Opcode) => <~~(opCode, o)

  // --- Push Rocks
  val PushRight = Pattern(OpcodePred('MoveRight),
                          Seq((0, 0) -> 'Robot, (1, 0) -> 'Rock,  (2, 0) -> 'Empty),
                          Seq((0, 0) -> 'Empty, (1, 0) -> 'Robot, (2, 0) -> 'Rock),
                          { s => s.copy(robotPos = s.robotPos + Coordinate(1, 0)) })

  val PushRightHO = Pattern(OpcodePred('MoveRight),
                            Seq((0, 0) -> 'Robot, (1, 0) -> 'HORock, (2, 0) -> 'Empty),
                            Seq((0, 0) -> 'Empty, (1, 0) -> 'Robot,  (2, 0) -> 'HORock),
                            { s => s.copy(robotPos = s.robotPos + Coordinate(1, 0)) })

  val PushLeft = Pattern(OpcodePred('MoveLeft),
                         Seq((-2, 0) -> 'Empty, (-1, 0) -> 'Rock,  (0, 0) -> 'Robot),
                         Seq((-2, 0) -> 'Rock,  (-1, 0) -> 'Robot, (0, 0) -> 'Empty),
                         { s => s.copy(robotPos = s.robotPos + Coordinate(-1, 0)) })

  val PushLeftHO = Pattern(OpcodePred('MoveLeft),
                           Seq((-2, 0) -> 'Empty,  (-1, 0) -> 'HORock, (0, 0) -> 'Robot),
                           Seq((-2, 0) -> 'HORock, (-1, 0) -> 'Robot,  (0, 0) -> 'Empty),
                           { s => s.copy(robotPos = s.robotPos + Coordinate(-1, 0)) })

  // --- The Falling of a Rock
  val Fall = Pattern((_, _) => true,
                     Seq((0, 0) -> 'Rock,   (0, 1) -> 'Empty),
                     Seq((0, 0) -> 'Empty, (0, 1) -> 'FallingRock))

  val FallHO = Pattern((_, _) => true,
                       Seq((0, 0) -> 'HORock, (0, 1) -> 'Empty),
                       Seq((0, 0) -> 'Empty,  (0, 1) -> 'HOFallingRock))

  val FallRight = Pattern((_, _) => true,
                  Seq((0, 0) -> 'Rock, (1, 0) -> 'Empty, (0, 1) -> 'Rock, (1, 1) -> 'Empty),
                  Seq((0, 0) -> 'Empty, (1, 1) -> 'FallingRock))

  val FallLeft = Pattern((_, _) => true,
                         Seq((-1, 0) -> 'Empty, (0, 0) -> 'Rock,  (-1, 1) -> 'Empty, (0, 1) -> 'Rock),
                         Seq((0, 0) -> 'Empty, (-1, 1) -> 'FallingRock))

  val FallRightR = Pattern((_, _) => true,
                           Seq((0, 0) -> 'Rock,  (1, 0) -> 'Empty, (0, 1) -> 'Lambda, (1, 1) -> 'Empty),
                           Seq((0, 0) -> 'Empty, ( 1, 1) -> 'FallingRock))

  val FallRightHO = Pattern((_, _) => true,
                            Seq(( 0, 0) -> 'HORock, (1, 0) -> 'Empty,  ( 0, 1) -> 'Rock,  (1, 1) -> 'Empty),
                            Seq((0, 0) -> 'Empty, (1, 1) -> 'HOFallingRock))

  val FallLeftHO = Pattern((_, _) => true,
                           Seq((-1, 0) -> 'Empty,  (0, 0) -> 'HORock, (-1, 1) -> 'Empty, (0, 1) -> 'Rock),
                           Seq((0, 0) -> 'Empty, (-1, 1) -> 'HOFallingRock))

  val FallRightRHO = Pattern((_, _) => true,
                             Seq((0, 0) -> 'HORock, (1, 0) -> 'Empty, (0, 1) -> 'Lambda, (1, 1) -> 'Empty),
                             Seq((0, 0) -> 'Empty, (1, 1) -> 'HOFallingRock))

  // --- Stabilization of Rocks when falling
  val StableW = Pattern((_, _) => true,
                        Seq((0, 0) -> 'FallingRock, (0, 1) -> 'Wall),
                        Seq((0, 0) -> 'Rock))

  val StableR = Pattern((_, _) => true,
                        Seq((0, 0) -> 'FallingRock, (0, 1) -> 'Rock),
                        Seq((0, 0) -> 'Rock))

  val StableL = Pattern((_, _) => true,
                        Seq((0, 0) -> 'FallingRock, (0, 1) -> 'Lambda),
                        Seq((0, 0) -> 'Rock))

  val StableOl = Pattern((_, _) => true,
                         Seq((0, 0) -> 'FallingRock, (0, 1) -> 'OpenLift),
                         Seq((0, 0) -> 'Rock))

  val StableCl = Pattern((_, _) => true,
                         Seq((0, 0) -> 'FallingRock, (0, 1) -> 'ClosedLift),
                         Seq((0, 0) -> 'Rock))

  val StableE = Pattern((_, _) => true,
                        Seq((0, 0) -> 'FallingRock, (0, 1) -> 'Earth),
                        Seq((0, 0) -> 'Rock))

  val StableWHO = Pattern((_, _) => true,
                          Seq((0, 0) -> 'HOFallingRock, (0, 1) -> 'Wall),
                          Seq((0, 0) -> 'Lambda))

  val StableRHO = Pattern((_, _) => true,
                          Seq((0, 0) -> 'HOFallingRock, (0, 1) -> 'Rock),
                          Seq((0, 0) -> 'Lambda))

  val StableLHO = Pattern((_, _) => true,
                          Seq((0, 0) -> 'HOFallingRock, (0, 1) -> 'Lambda),
                          Seq((0, 0) -> 'Lambda))

  val StableOlHO = Pattern((_, _) => true,
                           Seq((0, 0) -> 'HOFallingRock, (0, 1) -> 'OpenLift),
                           Seq((0, 0) -> 'Lambda))

  val StableClHO = Pattern((_, _) => true,
                           Seq((0, 0) -> 'HOFallingRock, (0, 1) -> 'ClosedLift),
                           Seq((0, 0) -> 'Lambda))

  val StableEHO = Pattern((_, _) => true,
                          Seq((0, 0) -> 'HOFallingRock, (0, 1) -> 'Earth),
                          Seq((0, 0) -> 'Lambda))

  // --- Losing Conditions
  val Die = Pattern((_, _) => true,
                    Seq((0, -1) -> 'FallingRock, (0, 0) -> 'Robot),
                    Seq(),
                    { s => s.copy(status = 'Lost) })

  val DieHO = Pattern((_, _) => true,
                      Seq((0, -1) -> 'HOFallingRock, (0, 0) -> 'Robot),
                      Seq(),
                      { s => s.copy(status = 'Lost) })

  val DieOutrun = Pattern(OpcodePred('MoveDown),
                          Seq((0, -1) -> 'Rock, (0, 0) -> 'Robot),
                          Seq(),
                          { s => s.copy(status = 'Lost) })

  val DieOutrunHO = Pattern(OpcodePred('MoveDown),
                            Seq((0, -1) -> 'HORock, (0, 0) -> 'Robot),
                            Seq(),
                            { s => s.copy(status = 'Lost) })

  // --- Winning Conditions
  val OpenGate = Pattern({(b, _) => b.tLambdas == b.lambdas },
                         Seq((0, 0) -> 'ClosedLift),
                         Seq((0, 0) -> 'OpenLift))

  // --- Movements
  val MvRight = Pattern(OpcodePred('MoveRight),
                        Seq((0, 0) -> 'Robot, (1, 0) -> 'Reachable),
                        Seq((0, 0) -> 'Empty, (1, 0) -> 'Robot),
                        { s => s.copy(robotPos = s.robotPos + Coordinate(1, 0)) })

  val MvLeft = Pattern(OpcodePred('MoveLeft),
                       Seq((-1, 0) -> 'Reachable, (0, 0) -> 'Robot),
                       Seq((-1, 0) -> 'Robot, (0, 0) -> 'Empty),
                       { s => s.copy(robotPos = s.robotPos + Coordinate(-1, 0)) })

  val MvUp = Pattern(OpcodePred('MoveUp),
                     Seq((0, -1) -> 'Reachable, (0, 0) -> 'Robot),
                     Seq((0, -1) -> 'Robot, (0, 0) -> 'Empty),
                     { s => s.copy(robotPos = s.robotPos + Coordinate(0, -1)) })

  val MvDown = Pattern(OpcodePred('MoveDown),
                       Seq((0, 0) -> 'Robot, (0, 1) -> 'Reachable),
                       Seq((0, 0) -> 'Empty, (0, 1) -> 'Robot),
                       { s => s.copy(robotPos = s.robotPos + Coordinate(0, 1)) })

  val MvRightWin = Pattern(OpcodePred('MoveRight),
                           Seq((0, 0) -> 'Robot, (1, 0) -> 'OpenLift),
                           Seq((0, 0) -> 'Empty, (1, 0) -> 'Robot),
                           { s => s.copy(robotPos = s.robotPos + Coordinate(1, 0), status = 'Win) })

  val MvLeftWin = Pattern(OpcodePred('MoveLeft),
                          Seq((-1, 0) -> 'OpenLift, (0, 0) -> 'Robot),
                          Seq((-1, 0) -> 'Robot, (0, 0) -> 'Empty),
                          { s => s.copy(robotPos = s.robotPos + Coordinate(-1, 0), status = 'Win) })

  val MvUpWin = Pattern(OpcodePred('MoveUp),
                        Seq((0, -1) -> 'OpenLift, (0, 0) -> 'Robot),
                        Seq((0, -1) -> 'Robot, (0, 0) -> 'Empty),
                        { s => s.copy(robotPos = s.robotPos + Coordinate(0, -1), status = 'Win) })

  val MvDownWin = Pattern(OpcodePred('MoveDown),
                          Seq((0, 0) -> 'Robot, (0, 1) -> 'OpenLift),
                          Seq((0, 0) -> 'Empty, (0, 1) -> 'Robot),
                          { s => s.copy(robotPos = s.robotPos + Coordinate(0, 1), status = 'Win) })

  val MvRightEat = Pattern(OpcodePred('MoveRight),
                           Seq((0, 0) -> 'Robot, (1, 0) -> 'Lambda),
                           Seq((0, 0) -> 'Empty, (1, 0) -> 'Robot),
                           { s => s.copy(lambdas = s.lambdas + 1, robotPos = s.robotPos + Coordinate(1, 0)) })

  val MvLeftEat = Pattern(OpcodePred('MoveLeft),
                          Seq((-1, 0) -> 'Lambda, (0, 0) -> 'Robot),
                          Seq((-1, 0) -> 'Robot, (0, 0) -> 'Empty),
                          { s => s.copy(lambdas = s.lambdas + 1, robotPos = s.robotPos + Coordinate(-1, 0)) })

  val MvUpEat = Pattern(OpcodePred('MoveUp),
                        Seq((0, -1) -> 'Lambda, (0, 0) -> 'Robot),
                        Seq((0, -1) -> 'Robot, (0, 0) -> 'Empty),
                        { s => s.copy(lambdas = s.lambdas + 1, robotPos = s.robotPos + Coordinate(0, -1)) })

  val MvDownEat = Pattern(OpcodePred('MoveDown),
                          Seq((0, 0) -> 'Robot, (0, 1) -> 'Lambda),
                          Seq((0, 0) -> 'Empty, (0, 1) -> 'Robot),
                          { s => s.copy(lambdas = s.lambdas + 1, robotPos = s.robotPos + Coordinate(0, 1)) })

  val MvRightRazor = Pattern(OpcodePred('MoveRight),
                             Seq((0, 0) -> 'Robot, (1, 0) -> 'Razor),
                             Seq((0, 0) -> 'Empty, (1, 0) -> 'Robot),
                             { s => s.copy(nRazors = s.nRazors + 1, robotPos = s.robotPos + Coordinate(1, 0)) })

  val MvLeftRazor = Pattern(OpcodePred('MoveLeft),
                            Seq((-1, 0) -> 'Razor, (0, 0) -> 'Robot),
                            Seq((-1, 0) -> 'Robot, (0, 0) -> 'Empty),
                            { s => s.copy(nRazors = s.nRazors + 1, robotPos = s.robotPos + Coordinate(-1, 0)) })

  val MvUpRazor = Pattern(OpcodePred('MoveUp),
                          Seq((0, -1) -> 'Razor, (0, 0) -> 'Robot),
                          Seq((0, -1) -> 'Robot, (0, 0) -> 'Empty),
                          { s => s.copy(nRazors = s.nRazors + 1, robotPos = s.robotPos + Coordinate(0, -1)) })

  val MvDownRazor = Pattern(OpcodePred('MoveDown),
                            Seq((0, 0) -> 'Robot, (0, 1) -> 'Razor),
                            Seq((0, 0) -> 'Empty, (0, 1) -> 'Robot),
                            { s => s.copy(nRazors = s.nRazors + 1, robotPos = s.robotPos + Coordinate(0, 1)) })

  // --- Trampolines
  def trampF(c: Coordinate)(s: Board) = {
    val newPos  = s.trampolines(s.robotPos + c)
    val sources = s.trampolines.filter(_._2 == newPos).map(_._1 -> 'Empty)
    s.copy(robotPos = newPos, tiles = (s.tiles + (newPos -> 'Robot)) ++ sources)
  }

  val MvRightT = Pattern(OpcodePred('MoveRight),
                         Seq((0, 0) -> 'Robot, (1, 0) -> 'Trampoline),
                         Seq((0, 0) -> 'Empty, (1, 0) -> 'Empty),
                         trampF(Coordinate(1, 0)))

  val MvLeftT = Pattern(OpcodePred('MoveLeft),
                        Seq((-1, 0) -> 'Trampoline, (0, 0) -> 'Robot),
                        Seq((-1, 0) -> 'Empty, (0, 0) -> 'Empty),
                        trampF(Coordinate(-1, 0)))

  val MvUpT = Pattern(OpcodePred('MoveUp),
                      Seq((0, -1) -> 'Trampoline, (0, 0) -> 'Robot),
                      Seq((0, -1) -> 'Empty, (0, 0) -> 'Empty),
                      trampF(Coordinate(-1, 0)))

  val MvDownT = Pattern(OpcodePred('MoveDown),
                        Seq((0, 0) -> 'Robot, (0, 1) -> 'Trampoline),
                        Seq((0, 0) -> 'Empty, (0, 1) -> 'Empty),
                        trampF(Coordinate(1, 0)))

  // --- Beards
  val beardGrowthPred = (b: Board, _: Opcode) => (b.tick % b.growth == (b.growth -1))

  val BeardGrowthN = Pattern(beardGrowthPred,
                             Seq((0, 0) -> 'Beard, (0, -1) -> 'Empty),
                             Seq(( 0, -1) -> 'Beard))

  val BeardGrowthNE = Pattern(beardGrowthPred,
                              Seq((0, 0) -> 'Beard, (1, -1) -> 'Empty),
                              Seq((1, -1) -> 'Beard))

  val BeardGrowthE = Pattern(beardGrowthPred,
                             Seq((0, 0) -> 'Beard, (1, 0) -> 'Empty),
                             Seq((1, 0) -> 'Beard))

  val BeardGrowthSE = Pattern(beardGrowthPred,
                              Seq((0, 0) -> 'Beard, (1, 1) -> 'Empty),
                              Seq((1, 1) -> 'Beard))

  val BeardGrowthS = Pattern(beardGrowthPred,
                             Seq((0, 0) -> 'Beard, (0,  1) -> 'Empty),
                             Seq((0, 1) -> 'Beard))

  val BeardGrowthSW = Pattern(beardGrowthPred,
                              Seq((0, 0) -> 'Beard, (-1, -1) -> 'Empty),
                              Seq((-1, -1) -> 'Beard))

  val BeardGrowthW = Pattern(beardGrowthPred,
                             Seq((0, 0) -> 'Beard, (-1, 0) -> 'Empty),
                             Seq((-1, 0) -> 'Beard))

  val BeardGrowthNW = Pattern(beardGrowthPred,
                              Seq((0, 0) -> 'Beard, (-1, 1) -> 'Empty),
                              Seq((-1, 1) -> 'Beard))

  val RazorBlowN = Pattern(OpcodePred('Razor),
                           Seq((0, 0) -> 'Robot, (0, -1) -> 'Beard),
                           Seq((0, -1) -> 'Empty))

  val RazorBlowNE = Pattern(OpcodePred('Razor),
                            Seq((0, 0) -> 'Robot, (1, -1) -> 'Beard),
                            Seq((1, -1) -> 'Empty))

  val RazorBlowE = Pattern(OpcodePred('Razor),
                           Seq((0, 0) -> 'Robot, (1, 0) -> 'Beard),
                           Seq((1, 0) -> 'Empty))

  val RazorBlowSE = Pattern(OpcodePred('Razor),
                            Seq((0, 0) -> 'Robot, (1, 1) -> 'Beard),
                            Seq((1, 1) -> 'Empty))

  val RazorBlowS = Pattern(OpcodePred('Razor),
                           Seq((0, 0) -> 'Robot, (0, 1) -> 'Beard),
                           Seq((0, 1) -> 'Empty))

  val RazorBlowSW = Pattern(OpcodePred('Razor),
                            Seq((0, 0) -> 'Robot, (-1, -1) -> 'Beard),
                            Seq((-1, -1) -> 'Empty))

  val RazorBlowW = Pattern(OpcodePred('Razor),
                           Seq((0, 0) -> 'Robot, (-1, 0) -> 'Beard),
                           Seq((-1, 0) -> 'Empty))

  val RazorBlowNW = Pattern(OpcodePred('Razor),
                            Seq((0, 0) -> 'Robot, (-1, 1) -> 'Beard),
                            Seq((-1, 1) -> 'Empty))

  val UseRazor = Pattern(OpcodePred('Razor),
                         Seq(),
                         Seq(),
                         { s => s.copy(nRazors = s.nRazors - 1) })

  val stabilization = List(StableW, StableR, StableL, StableOl, StableCl, StableE,
                           StableWHO, StableRHO, StableLHO, StableOlHO, StableClHO, StableEHO)

  val moves = List(MvRight, MvLeft, MvUp, MvDown, PushRight, PushLeft, MvRightWin, MvLeftWin,
                   MvUpWin, MvDownWin, MvRightEat, MvLeftEat, MvUpEat, MvDownEat,
                   MvRightRazor, MvLeftRazor, MvUpRazor, MvDownRazor,
                   MvRightT, MvLeftT, MvUpT, MvDownT)

  val fallsAndBeards = List(Fall, FallHO, FallRight, FallRightHO, FallRightR, FallRightRHO,
                            OpenGate, BeardGrowthN, BeardGrowthNE, BeardGrowthE, BeardGrowthSE,
                            BeardGrowthS, BeardGrowthSW, BeardGrowthW, BeardGrowthNW)

  val fallLefts = List(FallLeft, FallLeftHO)

  val razors = List(RazorBlowN, RazorBlowNE, RazorBlowE, RazorBlowSE, RazorBlowS,
                    RazorBlowSW, RazorBlowW, RazorBlowNW, Die, DieOutrun, DieHO, DieOutrunHO, UseRazor)

  // (pattern, tile filter condition)
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
}
