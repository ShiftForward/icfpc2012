import scala.collection.mutable.{HashMap => MutableMap}
import scala.collection.mutable
import collection.mutable.PriorityQueue
import scala.collection.mutable.Set
import scala.collection.mutable.Queue
import Coordinate.Implicits._
import Opcode._
import Tile._

object ShortestPathCalculator {
  type TTL = Int

  var liftPosition: Coordinate = _
  val possibleMoves = List('MoveUp, 'MoveDown, 'MoveLeft, 'MoveRight, 'Wait)
  val paths = MutableMap[(Coordinate, Coordinate), (TTL, List[Opcode])]()

  val defaultTTL = 1

  implicit def opcodeListToTTL(ops: List[Opcode]): (TTL, List[Opcode]) = (defaultTTL, ops)
  implicit def ttlTupleToOpcode(t: (TTL, List[Opcode])) = t._2

  private def applyMove(c: Coordinate, o: Opcode) = {
    o match {
      case 'MoveUp => c + Coordinate(0, -1)
      case 'MoveDown => c + Coordinate(0, 1)
      case 'MoveLeft => c + Coordinate(-1, 0)
      case 'MoveRight => c + Coordinate(1, 0)
      case _ => c
    }
  }

  implicit private def encodeBoard(b: Board): String = {
    b.robotPos.toString
  }

  private def boardHash(b: Board): String = {
    b.##.toString
  }

  private implicit def StateOrdering =
    new Ordering[(String, Int)] {
      def compare(
        a: (String, Int),
        b: (String, Int)) = b._2 - a._2
    }

  private def openLift(b: Board): Board = {
    b.copy(tiles = b.tiles ++ (if (liftPosition != null) Map(liftPosition -> 'OpenLift) else Map()))
  }

  private def cachedPath(b: Board, e: Coordinate) = {
    paths.get((b.robotPos, e)).map { case (ttl, ops) =>
      if (ttl == 0) {
        paths -= ((b.robotPos, e))
        false
      } else {
        paths += (((b.robotPos, e), (ttl - 1, ops)))
        true
      }
    } getOrElse(false)
  }

  private def cachePath(b: Board, e: Coordinate, ops: List[Opcode]) {
    if (ops.isEmpty)
      paths((b.robotPos, e)) = List()
    else
      ops.foldLeft((b, ops)) { case ((board, ops), op) =>
        paths((board.robotPos, e)) = ops
        (openLift(board.eval(op)), ops.tail)
      }
  }

  private def extractLiftPosition(board: Board) {
    val tile = board.tiles.find { case (coordinate, tile) =>
      tile match {
        case 'ClosedLift => true
        case _ => false
      }
    }

    liftPosition = tile match {
      case Some(entry) => entry._1
      case None => null
    }
  }

  def dijkstra(s: Coordinate, e: Coordinate, sb: Board): List[Opcode] = {
    extractLiftPosition(sb)

    val b = openLift(sb)
    val visitedStates = MutableMap[String, (List[Opcode], Board)]()
    val pq = PriorityQueue[(String, Int)]()
    pq += ((b, 0))
    visitedStates(b) = ((List(), b))

    while (!pq.isEmpty &&
           visitedStates(pq.head._1)._2.robotPos != e &&
           !cachedPath(visitedStates(pq.head._1)._2, e)) {
      val t = pq.dequeue()
      val c = t._1
      val (ops, b) = visitedStates(c)

      if (ops.size == t._2) {
        possibleMoves.foreach { m =>
          val rb = openLift(b.eval(m))
          val cd = ops.size + 1

          rb match {
            case rb if rb.status == 'Playing | rb.status == 'Win => {
              visitedStates.get(rb) match {
                case Some((currentOps, _)) if currentOps.size > cd => {
                  visitedStates(rb) = (m :: ops) -> rb
                  pq += ((rb, cd))
                }
                case None => {
                  visitedStates(rb) = (m :: ops) -> rb
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

    if (cachedPath(b, e))
      paths((b.robotPos, e))
    else if (pq.isEmpty) {
      cachePath(b, e, List())
      List()
    } else {
      val ops = visitedStates.get(pq.head._1) match {
        case Some((ops, _)) => ops.reverse
        case None => List()
      }

      cachePath(b, e, ops)
      ops
    }
  }
}
