case class Coordinate(x: Int, y: Int) {
  override def toString = "%s:%s".format(x, y)

  def +(that: Coordinate) = Coordinate(x + that.x, y + that.y)
  def -(that: Coordinate) = Coordinate(x - that.x, y - that.y)
  def *(factor: Double) = Coordinate((x * factor).toInt, (y * factor).toInt)

  def distance(that: Coordinate) = (this-that).length
  def length = math.sqrt(x * x + y * y)
}
