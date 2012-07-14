import collection.mutable
import scala.collection.immutable.TreeMap
import scala.collection.mutable.{Map => MutableMap}
import java.security.MessageDigest
import Coordinate.Implicits._

object ShortestPathCalculator {
  private lazy val messageDigest = MessageDigest.getInstance("MD5")
  private lazy val mapEncodings = MutableMap[String, String]()

  implicit private def encodeBoard(b: Board): String = {
    b.robotPos.toString
  }

  implicit def ShortestPathOrdering =
    new Ordering[(Coordinate, Int)] {
      def compare(
        a: (Coordinate, Int),
        b: (Coordinate, Int)) = b._2 - a._2
    }

  def possibleMoves = List(MoveUp(), MoveDown(), MoveLeft(), MoveRight(), Wait())

  def shortestPath(s: Coordinate, e: Coordinate, sb: Board): List[Opcode] = {
    val rb = sb.copy(robotPos = s)
    val visitedStates =
      MutableMap[String, (List[Opcode], Board)]()
    val pq = mutable.PriorityQueue[(String, Int)]()
    pq += ((sb, 0))
    visitedStates(sb) = (List() -> rb)

    while (!pq.isEmpty && visitedStates(pq.head._1)._2.robotPos != e) {
      val t = pq.dequeue()
      val c = t._1
      val (ops, b) = visitedStates(c)

      if (ops.size == t._2) {
        possibleMoves.foreach { m =>
          val rb = b.eval(m)
          rb match {
            case rb: Board if rb.status == Playing() => {
              visitedStates.get(rb) match {
                case Some((l, _)) if l.size >= ops.size + 1 => {
                  visitedStates(rb) = (m :: ops) -> rb
                  pq += ((rb, (ops.size + 1)))
                }
                case None => {
                  visitedStates(rb) = (m :: ops) -> rb
                  pq += ((rb, (ops.size + 1)))
                }
                case _ => // do nothing
              }
            }
            case _ => // do nothing
          }
        }
      }
    }

    if (pq.isEmpty)
      List()
    else
      visitedStates.get(pq.head._1) match {
        case Some((ops, _)) => ops.reverse
        case None => List()
      }
  }

  def shortestPath(e: Coordinate, b: Board): List[Opcode] =
    shortestPath(b.robotPos, e, b)
}
