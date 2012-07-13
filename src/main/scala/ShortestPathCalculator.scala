import scala.Tuple2
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.{Map => MutableMap}

object ShortestPathCalculator {
  implicit def ShortestPathOrdering =
    new Ordering[Tuple2[Coordinate, Int]] {
      def compare(
        a: Tuple2[Coordinate, Int],
        b: Tuple2[Coordinate, Int]) = b._2 - a._2
    }

  def possibleMoves = List(Up(), Down(), Left(), Right(), Wait())

  def shortestPath(s: Coordinate, e: Coordinate, sb: PlayingBoard): List[Opcode] = {
    val rb = sb.copy(robotPos = s)
    val visitedCoordinates =
      MutableMap[Coordinate, Tuple2[List[Opcode], Board]]()
    val pq = PriorityQueue[Tuple2[Coordinate, Int]]()
    pq += (s -> 0)
    visitedCoordinates(s) = (List() -> rb)

    while (!pq.isEmpty && pq.head != e) {
      val t = pq.dequeue
      val c = t._1
      val (ops, b) = visitedCoordinates(c)

      if (ops.size == t._2) {
        possibleMoves.foreach { m =>
          val rb = VM.eval(m, b)
          val nc = rb.robotPos

          rb.tiles.get(nc) match {
            case Some(v) => v match {
              case t: Reachable => {
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
}
