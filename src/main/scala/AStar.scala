import scala.collection.mutable.{HashMap => MutableMap}
import collection.mutable.PriorityQueue
import scala.collection.immutable.TreeMap
import Coordinate.Implicits._
import Tile._
import Opcode._

case class ExtendedBoard(board: Board, pathToLift: List[Coordinate])

object AStar {
  var liftPosition: Coordinate = _

  private def evaluateState(ops: List[Opcode], b: ExtendedBoard) = {
    var evaluation = ops.size - b.board.lambdas * 25

    if (b.board.status == Win()) {
      evaluation -= b.board.lambdas * 50
    } else if (b.board.tLambdas == b.board.lambdas) {
      evaluation += b.board.liftPosition.manhattanDistance(b.board.robotPos)
    } else {
      evaluation += b.board.allLambdas.foldLeft(b.board.width + b.board.height) { (minval, coordinate) =>
        val dist = coordinate.manhattanDistance(b.board.robotPos)
        if (dist < minval)
          dist
        else
          minval
      }
    }

    if (b.pathToLift.isEmpty) {
      evaluation += 50 * b.board.tLambdas
    }

    evaluation
  }

  private def extractLiftPosition(b: Board) {
    liftPosition = b.tiles.find({ entry => <~(entry._2, 'Lift) }).get._1
  }

  private def evaluateScore(b: ExtendedBoard) = {
    b.board.lambdas
  }

  private def extendFromBoard(b: Board) = {
    ExtendedBoard(b, ShortestPathCalculator.bfs(b.robotPos, liftPosition, b))
  }

  private def nextExtendedBoard(b: ExtendedBoard, o: Opcode) = {
    ExtendedBoard(b.board.eval(o),
                  if (!b.pathToLift.isEmpty && ShortestPathCalculator.isClear(b.pathToLift, b.board))
                    ShortestPathCalculator.bfs(b.board.robotPos, b.pathToLift.head, b.board) ++ b.pathToLift.tail
                  else
                    ShortestPathCalculator.bfs(b.board.robotPos, liftPosition, b.board))
  }

  implicit private def encodeBoard(b: ExtendedBoard): String = {
    b.board.tiles.##.toString
  }

  private implicit def StateOrdering =
    new Ordering[(String, Int)] {
      def compare(
        a: (String, Int),
        b: (String, Int)) = b._2 - a._2
    }

  val possibleMoves = List('MoveUp, 'MoveDown, 'MoveLeft, 'MoveRight, 'Wait)

  def evaluateBestSolution(sb: Board) = {
    extractLiftPosition(sb)

    val visitedStates = MutableMap[String, (List[Opcode], ExtendedBoard)]()
    val pq = PriorityQueue[(String, Int)]()
    val boardEvaluations = MutableMap[String, Int]()
    val b = extendFromBoard(sb)
    var bestSoFar = List[Opcode]()
    var bestScore = 0
    pq += ((b, evaluateState(bestSoFar, b)))
    visitedStates(b) = (bestSoFar -> b)
    boardEvaluations(b) = evaluateState(bestSoFar, b)

    val startTime = System.nanoTime()

    while (!pq.isEmpty &&
           visitedStates(pq.head._1)._2.board.status != Win() &&
           (System.nanoTime() - startTime) / 1000000 < 100000) {
      val t = pq.dequeue()
      val c = t._1
      val (ops, b) = visitedStates(c)
      val dd = boardEvaluations(c)

      if (dd == t._2) {
        possibleMoves.foreach { m =>
          val neb = nextExtendedBoard(b, m)
          val cd = evaluateState(m :: ops, neb)

          if (evaluateScore(neb) < bestScore) {
            bestSoFar = m :: ops
            bestScore = evaluateScore(neb)
          }

          neb match {
            case ExtendedBoard(rb, _) if rb.status == Playing() | rb.status == Win() => {
              boardEvaluations.get(neb) match {
                case Some(d) if d > cd => {
                  visitedStates(neb) = (m :: ops) -> neb
                  boardEvaluations(neb) = cd
                  pq += ((neb, cd))
                }
                case None => {
                  visitedStates(neb) = (m :: ops) -> neb
                  boardEvaluations(neb) = cd
                  pq += ((neb, cd))
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
