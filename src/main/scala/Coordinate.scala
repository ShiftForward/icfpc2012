final case class Coordinate(x: Int, y: Int) {
  override def toString = "%s:%s".format(x, y)

  @inline def +(that: Coordinate) = Coordinate(x + that.x, y + that.y)
  @inline def -(that: Coordinate) = Coordinate(x - that.x, y - that.y)
  @inline def *(factor: Double)   = Coordinate((x * factor).toInt, (y * factor).toInt)

  def distance(that: Coordinate) = (this-that).length
  def manhattanDistance(that: Coordinate) = math.abs(this.x - that.x) + math.abs(this.y - that.y)

  def length = math.sqrt(x * x + y * y)

  @inline def Up    = Coordinate(0, y - 1)
  @inline def Down  = Coordinate(0, y + 1)
  @inline def Left  = Coordinate(x - 1, 0)
  @inline def Right = Coordinate(x + 1, 0)

  @inline def isInside(width: Int, height: Int) = x >= 0 && x <= width && y >= 0 && y <= height
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
