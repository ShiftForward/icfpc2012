object Tile {
  type Tile = Symbol

  def <~(a: Tile, b: Tile): Boolean = {
    (a == b) ||
    (b == 'Tile) ||
    (a == 'Earth && b == 'Reachable) ||
    (a == 'Empty && b == 'Reachable) ||
    (a == 'OpenLift && b == 'Lift) ||
    (a == 'ClosedLift && b == 'Lift) ||
    (a == 'StableRock && b == 'Rock) ||
    (a == 'FallingRock && b == 'Rock)
  }

  def apply(c: Char): Symbol = {
    c match {
      case 'R'  => 'Robot
      case '#'  => 'Wall
      case '*'  => 'StableRock
      case '\\' => 'Lambda
      case 'L'  => 'ClosedLift
      case 'O'  => 'OpenLift
      case '.'  => 'Earth
      case ' '  => 'Empty
    }
  }
}
