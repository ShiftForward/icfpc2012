import collection.mutable
import scala.collection.immutable.TreeMap
import scala.collection.mutable.{Map => MutableMap}
import java.security.MessageDigest
import Coordinate.Implicits._

object ShortestPathCalculator {
  private lazy val messageDigest = MessageDigest.getInstance("MD5")
  private lazy val mapEncodings = MutableMap[String, String]()
  private lazy val pathCaches = MutableMap[Coordinate, Map[Coordinate, List[Opcode]]]()

  implicit private def encodeBoard(b: Board): String = {
    b.robotPos.toString
  }

  private implicit def ShortestPathOrdering =
    new Ordering[(Coordinate, Int)] {
      def compare(
        a: (Coordinate, Int),
        b: (Coordinate, Int)) = b._2 - a._2
    }

  private def possibleMoves = List(MoveUp(), MoveDown(), MoveLeft(), MoveRight(), Wait())

  def shortestPath(s: Coordinate, e: Coordinate, sb: Board): List[Opcode] = {
    val rb = sb.copy(robotPos = s)
    val visitedStates =
      MutableMap[String, (List[Opcode], Board)]()
    val nodeDistances = MutableMap[String, Int]()
    val pq = mutable.PriorityQueue[(String, Int)]()
    pq += ((sb, s.distance(e).toInt))
    visitedStates(sb) = (List() -> rb)
    nodeDistances(sb) = s.distance(e).toInt

    while (!pq.isEmpty && visitedStates(pq.head._1)._2.robotPos != e) {
      val t = pq.dequeue()
      val c = t._1
      val (ops, b) = visitedStates(c)
      val dd = nodeDistances(c)

      if (dd == t._2) {
        possibleMoves.foreach { m =>
          val rb = b.eval(m)
          val cd = ops.size + 1 + rb.robotPos.distance(e).toInt

          rb match {
            case rb: Board if rb.status == Playing() => {
              nodeDistances.get(rb) match {
                case Some(d) if d >= cd => {
                  visitedStates(rb) = (m :: ops) -> rb
                  nodeDistances(rb) = cd
                  pq += ((rb, cd))
                }
                case None => {
                  visitedStates(rb) = (m :: ops) -> rb
                  nodeDistances(rb) = cd
                  pq += ((rb, cd))
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
