import scala.collection.mutable
import scala.collection.mutable.Set
import scala.collection.mutable.Queue
import Coordinate.Implicits._
import Opcode._
import Tile._

object ShortestPathCalculator {
  val possibleMoves = List('MoveUp, 'MoveDown, 'MoveLeft, 'MoveRight)

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
}
