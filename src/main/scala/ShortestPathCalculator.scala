import scala.collection.mutable.{HashMap => MutableMap}
import scala.collection.mutable
import collection.mutable.PriorityQueue
import scala.collection.mutable.Set
import scala.collection.mutable.Queue
import Coordinate.Implicits._
import Opcode._
import Tile._

object ShortestPathCalculator {
  val possibleMoves = List('MoveUp, 'MoveDown, 'MoveLeft, 'MoveRight, 'Wait)
  lazy val openedBoards = MutableMap[String, Board]()
  lazy val paths = MutableMap[(Coordinate, Coordinate), List[Opcode]]()

  private def applyMove(c: Coordinate, o: Opcode) = {
    o match {
      case 'MoveUp => c + Coordinate(0, -1)
      case 'MoveDown => c + Coordinate(0, 1)
      case 'MoveLeft => c + Coordinate(-1, 0)
      case 'MoveRight => c + Coordinate(1, 0)
      case _ => c
    }
  }

  def bfs(s: Coordinate, e: Coordinate, b: Board): List[Coordinate] = {
    val visited = Set[Coordinate]()
    val q = Queue[List[Coordinate]]()
    q.enqueue(List(s))
    visited += s

    while (!q.isEmpty && q.head.head != e) {
      val path = q.dequeue()

      possibleMoves.foreach { m =>
        val nc = applyMove(path.head, m)
        if (!visited.contains(nc) && (<~(b.get(nc), 'Reachable) ||
                                      <~(b.get(nc), 'Lambda) ||
                                      <~(b.get(nc), 'Lift))) {
          visited += nc
          q.enqueue(nc :: path)
        }
      }
    }

    if (q.isEmpty)
      List()
    else
      q.head.reverse
  }

  def isClear(cs: List[Coordinate], b: Board) = {
    cs.foldLeft(true) { (clear, c) =>
      clear && (<~(b.get(c), 'Reachable) ||
                <~(b.get(c), 'Lambda) ||
                <~(b.get(c), 'Lift))
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
    openedBoards.get(boardHash(b)) match {
      case Some(board) => board
      case None => {
        val opened = b.copy(tiles = b.tiles map { case (coordinate, tile) =>
          tile match {
            case 'ClosedLift => (coordinate, 'OpenLift)
              case _ => (coordinate, tile)
          }
        })
        openedBoards(boardHash(b)) = opened
        opened
      }
    }
  }

  private def cachedPath(b: Board, e: Coordinate) = {
    if (paths.contains((b.robotPos, e))) {
      val ops = paths((b.robotPos, e))
      val nb = ops.foldLeft(openLift(b)) { (board, op) =>
        openLift(board.eval(op))
      }
      if (nb.robotPos == e)
        true
      else {
        paths -= ((b.robotPos, e))
        false
      }
    } else {
      false
    }
  }

  private def cachePath(b: Board, e: Coordinate, ops: List[Opcode]) {
    paths((b.robotPos, e)) = ops
  }

  def dijkstra(s: Coordinate, e: Coordinate, sb: Board): List[Opcode] = {
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
      cachePath(sb, b.robotPos, ops)

      if (ops.size == t._2) {
        possibleMoves.foreach { m =>
          val rb = openLift(b.eval(m))
          val cd = ops.size + 1

          rb match {
            case rb if rb.status == Playing() | rb.status == Win() => {
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

    if (cachedPath(sb, e))
      paths((sb.robotPos, e))
    else if (pq.isEmpty)
      List()
    else
      visitedStates.get(pq.head._1) match {
        case Some((ops, _)) => ops.reverse
        case None => List()
      }
  }
}
