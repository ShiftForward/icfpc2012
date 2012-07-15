object Tile {
  type Tile = Symbol

  def <~(a: Tile, b: Tile): Boolean = {
    (a == b) ||
    (b == 'Tile) ||
    (b == 'Reachable && (a == 'Earth        || a == 'Empty)) ||
    (b == 'Lift      && (a == 'OpenLift     || a == 'ClosedLift)) ||
    (b == 'Rock      && (a == 'StableRock   || a == 'FallingRock)) ||
    (b == 'HORock    && (a == 'HOStableRock || a == 'HOFallingRock))
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
    }
  }
}