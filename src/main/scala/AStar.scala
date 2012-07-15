import scala.collection.mutable.{HashMap => MutableMap}
import collection.mutable.PriorityQueue
import scala.collection.immutable.TreeMap
import Coordinate.Implicits._
import Opcode._

object AStar {
  def evaluateState(ops: List[Opcode], b: Board) = {
    ops.size + (b.tLambdas - b.lambdas)
  }

  implicit private def encodeBoard(b: Board): String = {
    b.##.toString
  }

  private implicit def StateOrdering =
    new Ordering[(String, Int)] {
      def compare(
        a: (String, Int),
        b: (String, Int)) = b._2 - a._2
    }

  val possibleMoves = List('MoveUp, 'MoveDown, 'MoveLeft, 'MoveRight, 'Wait)

  def evaluateBestSolution(b: Board) = {
    val visitedStates = MutableMap[String, (List[Opcode], Board)]()
    val pq = PriorityQueue[(String, Int)]()
    val boardEvaluations = MutableMap[String, Int]()
    pq += ((b, evaluateState(List(), b)))
    visitedStates(b) = (List() -> b)
    boardEvaluations(b) = evaluateState(List(), b)

    while (!pq.isEmpty && visitedStates(pq.head._1)._2.status != Win()) {
      val t = pq.dequeue()
      val c = t._1
      val (ops, b) = visitedStates(c)
      val dd = boardEvaluations(c)

      if (dd == t._2) {
        possibleMoves.foreach { m =>
          val rb = b.eval(m)
          val cd = evaluateState(m :: ops, rb)

          rb match {
            case rb: Board if rb.status == Playing() | rb.status == Win() => {
              boardEvaluations.get(rb) match {
                case Some(d) if d > cd => {
                  visitedStates(rb) = (m :: ops) -> rb
                  boardEvaluations(rb) = cd
                  pq += ((rb, cd))
                }
                case None => {
                  visitedStates(rb) = (m :: ops) -> rb
                  boardEvaluations(rb) = cd
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
}
