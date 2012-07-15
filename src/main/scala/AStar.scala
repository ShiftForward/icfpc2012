import scala.collection.mutable.{HashMap => MutableMap}
import collection.mutable.PriorityQueue
import scala.collection.immutable.TreeMap
import Coordinate.Implicits._
import Opcode._

object AStar {
  private def evaluateState(ops: List[Opcode], b: Board) = {
    var evaluation = ops.size - b.lambdas * 25

    if (b.status == Win()) {
      evaluation -= b.lambdas * 50
    } else if (b.tLambdas == b.lambdas) {
      evaluation += b.liftPosition.manhattanDistance(b.robotPos)
    } else {
      evaluation += b.allLambdas.foldLeft(b.width + b.height) { (minval, coordinate) =>
        val dist = coordinate.manhattanDistance(b.robotPos)
        if (dist < minval)
          dist
        else
          minval
      }
    }

    evaluation
  }

  private def evaluateScore(b: Board) = {
    b.lambdas
  }

  implicit private def encodeBoard(b: Board): String = {
    b.tiles.##.toString
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
    var bestSoFar = List[Opcode]()
    var bestScore = 0
    pq += ((b, evaluateState(bestSoFar, b)))
    visitedStates(b) = (bestSoFar -> b)
    boardEvaluations(b) = evaluateState(bestSoFar, b)

    val startTime = System.nanoTime()

    while (!pq.isEmpty && visitedStates(pq.head._1)._2.status != Win() && (System.nanoTime() - startTime) / 1000000 < 100000) {
      val t = pq.dequeue()
      val c = t._1
      val (ops, b) = visitedStates(c)
      val dd = boardEvaluations(c)

      if (dd == t._2) {
        possibleMoves.foreach { m =>
          val rb = b.eval(m)
          val cd = evaluateState(m :: ops, rb)

          if (evaluateScore(rb) < bestScore) {
            bestSoFar = m :: ops
            bestScore = evaluateScore(rb)
          }

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
      bestSoFar
    else
      visitedStates.get(pq.head._1) match {
        case Some((ops, _)) => ops.reverse
        case None => List()
      }
  }
}
