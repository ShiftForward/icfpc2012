final case class Coordinate(x: Int, y: Int) {
  override def toString = "%s:%s".format(x, y)

  @inline def +(that: Coordinate) = Coordinate(x + that.x, y + that.y)
  @inline def -(that: Coordinate) = Coordinate(x - that.x, y - that.y)

  def distance(that: Coordinate) = (this-that).length
  def manhattanDistance(that: Coordinate) = math.abs(this.x - that.x) + math.abs(this.y - that.y)

  def length = math.sqrt(x * x + y * y)
}

object Coordinate {
  def apply(tuple: (Int, Int)): Coordinate = Coordinate(tuple._1, tuple._2)

  object Implicits {
    implicit def tupleToCoordinate(t: (Int, Int)): Coordinate = Coordinate(t)

    implicit val coordinateOrdering = new Ordering[Coordinate] {
      def compare(c1: Coordinate, c2: Coordinate) = {
        if (c1.y > c2.y || c1.y == c2.y && c1.x > c2.x) 1
        else if (c1.y < c2.y || c1.y == c2.y && c1.x < c2.x) -1
        else 0
      }
    }
  }
}
