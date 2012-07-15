object Tile {
  type Tile = Symbol

  def <~(a: Tile, b: Tile): Boolean = {
    (a == b) ||
    (b == 'Tile) ||
    (b == 'Reachable  && (a == 'Earth           || a == 'Empty)) ||
    (b == 'Lift       && (a == 'OpenLift        || a == 'ClosedLift)) ||
    (b == 'Rock       && (a == 'FallingRock))   ||
    (b == 'HORock     && (a == 'HOFallingRock)) ||
    (b == 'Trampoline && (a.toString().startsWith("'Trampoline"))) ||
    (b == 'Target     && (a.toString().startsWith("'Target")))
  }

  def apply(c: Char): Symbol = {
    c match {
      case 'R'  => 'Robot
      case '#'  => 'Wall
      case '*'  => 'Rock
      case '\\' => 'Lambda
      case 'L'  => 'ClosedLift
      case 'O'  => 'OpenLift
      case '.'  => 'Earth
      case ' '  => 'Empty
      case 'W'  => 'Beard
      case '@'  => 'HORock
      case '!'  => 'Razor
      case 'A'  => 'TrampolineA
      case 'B'  => 'TrampolineB
      case 'C'  => 'TrampolineC
      case 'D'  => 'TrampolineD
      case 'E'  => 'TrampolineE
      case 'F'  => 'TrampolineF
      case 'G'  => 'TrampolineG
      case 'H'  => 'TrampolineH
      case 'I'  => 'TrampolineI
      case '1'  => 'Target1
      case '2'  => 'Target2
      case '3'  => 'Target3
      case '4'  => 'Target4
      case '5'  => 'Target5
      case '6'  => 'Target6
      case '7'  => 'Target7
      case '8'  => 'Target8
      case '9'  => 'Target9
    }
  }
}
