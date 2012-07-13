import collection.mutable
import scala.collection.immutable.TreeMap
import scala.collection.mutable.{Map => MutableMap}
import java.security.MessageDigest
import Coordinate.Implicits._

object ShortestPathCalculator {
  private lazy val messageDigest = MessageDigest.getInstance("MD5")
  private lazy val mapEncodings = MutableMap[String, String]()

  implicit private def encodeBoard(b: Board): String = {
//    val bs = b.robotPos.toString

    val lines = TreeMap(b.tiles.toArray: _*).groupBy { case (pos, _) => pos.y }
    val sortedLines = TreeMap(lines.toArray: _*)

    val bs = sortedLines.map { case (n, line) =>
      line.map { case (_, tile) =>
        tile match {
          case _: Robot => 'R'
          case _: Wall => '#'
          case _: Lambda => ' '
          case _: Earth => ' '
          case _: Empty => ' '
          case _: ClosedLift => ' '
          case _: OpenLift => ' '
          case _: Rock => '#'
          case _ => ' '
        }
      }.mkString

    }.mkString("\n")

    mapEncodings.get(bs) match {
      case Some(s) => s
      case None => {
        val e = messageDigest.digest(bs.getBytes).map("%02x".format(_)).mkString
        mapEncodings(bs) = e
        e
      }
    }
  }

  implicit def ShortestPathOrdering =
    new Ordering[(Coordinate, Int)] {
      def compare(
        a: (Coordinate, Int),
        b: (Coordinate, Int)) = b._2 - a._2
    }

  def possibleMoves = List(MoveUp(), MoveDown(), MoveLeft(), MoveRight(), Wait())

  def shortestPath(s: Coordinate, e: Coordinate, sb: PlayingBoard): List[Opcode] = {
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
            case rb: PlayingBoard => {
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

  def shortestPath(e: Coordinate, b: PlayingBoard): List[Opcode] =
    shortestPath(b.robotPos, e, b)
}
