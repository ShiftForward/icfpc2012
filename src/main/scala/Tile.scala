sealed trait Tile {
  def pos: Coordinate
}

case class Robot(pos: Coordinate) extends Tile
case class Wall(pos: Coordinate) extends Tile
case class Lambda(pos: Coordinate) extends Tile
case class Earth(pos: Coordinate) extends Tile
case class Empty(pos: Coordinate) extends Tile
sealed trait Lift extends Tile
case class OpenLift(pos: Coordinate) extends Lift
case class ClosedLift(pos: Coordinate) extends Lift
sealed trait Rock extends Tile
case class StableRock(pos: Coordinate) extends Rock
case class FallingRock(pos: Coordinate) extends Rock

case class Board(width: Int, height: Int, tiles: Vector[Tile]) {
  def contains(pos: Coordinate) = pos.isInside(width, height)
}
