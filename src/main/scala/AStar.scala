import scala.collection.mutable.{HashMap => MutableMap}
import collection.mutable.PriorityQueue
import scala.collection.immutable.TreeMap
import Coordinate.Implicits._
import Tile._
import Opcode._

case class ExtendedBoard(board: Board)

object AStar {
  var liftPosition: Coordinate = _
  var coordinates: Set[Coordinate] = _
  val evaluations = MutableMap[String, Int]()

  implicit private def encodeBoard(b: ExtendedBoard): String = {
    b.board.tiles.##.toString
  }

  private val points = Map(
    'Lambda -> 25,
    'ClosedLift -> 50,
    'OpenLift -> 50)

  private def evaluateState(ops: List[Opcode], b: ExtendedBoard) = {
    var evaluation = ops.size * 10

    var mapEval = evaluations.get(b) match {
      case Some(value) => value
      case None => {
        var m = -b.board.lambdas * 25

        if (b.board.status == Win()) {
          m -= b.board.lambdas * 50
        } else if (b.board.tLambdas == b.board.lambdas) {
          m += liftPosition.manhattanDistance(b.board.robotPos)
        } else {
          m += 25 * (b.board.tLambdas - b.board.lambdas) * b.board.allLambdas.foldLeft(b.board.width + b.board.height) { (minval, coordinate) =>
            val dist = coordinate.manhattanDistance(b.board.robotPos)
            if (dist < minval)
              dist
            else
              minval
          }
        }

        val coords = FloodFiller.fill(b.board.robotPos, b.board)
        m += (coordinates -- coords).foldLeft(0) { (sum, coord) =>
          points.get(b.board.get(coord)) match {
            case Some(tileValue) => sum + tileValue
            case None => sum
          }
        }

        evaluations(b) = m
        m
      }
    }

    evaluation + mapEval
  }

  private def extractLiftPosition(b: Board) {
    liftPosition = b.tiles.find({ entry => <~(entry._2, 'Lift) }).get._1
  }

  private def extractCoordinates(b: Board) {
    coordinates = b.tiles.foldLeft(Set[Coordinate]()) { (set, entry) =>
      set + entry._1
    }
  }

  private def evaluateScore(b: ExtendedBoard) = {
    var score = b.board.lambdas * 25
    if (b.board.status == Win())
      score += b.board.lambdas * 50
    else
      0
    score -= b.board.tick
    score
  }

  private def extendFromBoard(b: Board) = {
    ExtendedBoard(b)
  }

  private def nextExtendedBoard(b: ExtendedBoard, o: Opcode) = {
    val nb = b.board.eval(o)
    ExtendedBoard(nb)
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
    extractCoordinates(sb)

    val visitedStates = MutableMap[String, (List[Opcode], ExtendedBoard)]()
    val pq = PriorityQueue[(String, Int)]()
    val boardEvaluations = MutableMap[String, Int]()
    val b = extendFromBoard(sb)
    var bestSoFar = List[Opcode]()
    var bestScore = evaluateScore(b)
    pq += ((b, evaluateState(bestSoFar, b)))
    visitedStates(b) = (bestSoFar -> b)
    boardEvaluations(b) = evaluateState(bestSoFar, b)

    val startTime = System.nanoTime()

    while (!pq.isEmpty &&
           visitedStates(pq.head._1)._2.board.status != Win() &&
           (System.nanoTime() - startTime) / 1000000 < 10000) {
      val t = pq.dequeue()
      val c = t._1
      val (ops, b) = visitedStates(c)
      val dd = boardEvaluations(c)

      if (dd == t._2) {
        possibleMoves.foreach { m =>
          val neb = nextExtendedBoard(b, m)
          val cd = evaluateState(m :: ops, neb)

          if (evaluateScore(neb) > bestScore) {
            bestSoFar = m :: ops
            bestScore = evaluateScore(neb)
            println("Best so far (" + bestScore + ") = " + Opcode.toString(bestSoFar))
          }

          neb match {
            case ExtendedBoard(rb) if rb.status == Playing() | rb.status == Win() => {
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
