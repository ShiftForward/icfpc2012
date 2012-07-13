import collection.mutable
import scala.collection.mutable.{Map => MutableMap}

object ShortestPathCalculator {
  implicit def ShortestPathOrdering =
    new Ordering[(Coordinate, Int)] {
      def compare(
        a: (Coordinate, Int),
        b: (Coordinate, Int)) = b._2 - a._2
    }

  def possibleMoves = List(MoveUp(), MoveDown(), MoveLeft(), MoveRight(), Wait())

  def shortestPath(s: Coordinate, e: Coordinate, sb: PlayingBoard): List[Opcode] = {
    val rb = sb.copy(robotPos = s)
    val visitedCoordinates =
      MutableMap[Coordinate, (List[Opcode], Board)]()
    val pq = mutable.PriorityQueue[(Coordinate, Int)]()
    pq += (s -> 0)
    visitedCoordinates(s) = (List() -> rb)

    while (!pq.isEmpty && pq.head != e) {
      val t = pq.dequeue()
      val c = t._1
      val (ops, b) = visitedCoordinates(c)

      if (ops.size == t._2) {
        possibleMoves.foreach { m =>
          val rb = b.eval(m)
          rb match {
            case rb: PlayingBoard => {
              val nc = rb.robotPos

              visitedCoordinates.get(nc) match {
                case Some((l, _)) if l.size >= ops.size + 1 => {
                  visitedCoordinates(nc) = (m :: ops) -> rb
                  pq += (nc -> (ops.size + 1))
                }
                case None => {
                  visitedCoordinates(nc) = (m :: ops) -> rb
                  pq += (nc -> (ops.size + 1))
                }
                case _ => // do nothing
              }
            }
            case _ => // do nothing
          }
        }
      }
    }

    visitedCoordinates.get(e) match {
      case Some((ops, _)) => ops.reverse
      case None => List()
    }
  }

  def shortestPath(e: Coordinate, b: PlayingBoard): List[Opcode] =
    shortestPath(b.robotPos, e, b)
}
