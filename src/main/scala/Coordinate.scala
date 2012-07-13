case class Coordinate(x: Int, y: Int) {
  override def toString = "%s:%s".format(x, y)

  def +(that: Coordinate) = Coordinate(x + that.x, y + that.y)
  def -(that: Coordinate) = Coordinate(x - that.x, y - that.y)
  def *(factor: Double)   = Coordinate((x * factor).toInt, (y * factor).toInt)

  def distance(that: Coordinate) = (this-that).length
  def length = math.sqrt(x * x + y * y)

  def Up    = this + Coordinate( 0, -1)
  def Down  = this + Coordinate( 0,  1)
  def Left  = this + Coordinate(-1,  0)
  def Right = this + Coordinate( 1,  0)

  def isInside(width: Int, height: Int) = x >= 0 && x <= width && y >= 0 && y <= width
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
