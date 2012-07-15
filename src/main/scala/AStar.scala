import scala.collection.mutable.{HashMap => MutableMap}
import collection.mutable.PriorityQueue
import scala.collection.immutable.TreeMap
import Coordinate.Implicits._
import Tile._
import Opcode._

object AStar {
  var liftPosition: Coordinate = _
  var allLambdas: List[Coordinate] = _
  var coordinates: Set[Coordinate] = _
  val evaluations = MutableMap[String, Int]()
  val bestInCoord = MutableMap[Coordinate, Int]()

  implicit private def encodeBoard(b: Board): String = {
    b.tiles.##.toString
  }

  private val points = Map(
    'Lambda -> 25,
    'ClosedLift -> 50,
    'OpenLift -> 50)

  private def evaluateOpcodeList(ops: List[Opcode]) = {
    ops.size
  }

  private def evaluateBoard(b: Board) = {
    evaluations.get(b) match {
      case Some(value) => value
      case None => {
        var m = -b.lambdas * 25

        if (b.status == Win()) {
          m -= b.lambdas * 50
        } else if (b.tLambdas == b.lambdas) {
          m += liftPosition.manhattanDistance(b.robotPos)
        } else {
          m += allLambdas.foldLeft(b.width + b.height) { (minval, coordinate) =>
            if (b.get(coordinate) == 'Lambda) {
            val dist = coordinate.manhattanDistance(b.robotPos)
            if (dist < minval)
              dist
            else
              minval
            } else
              minval
          }
        }

        val coords = FloodFiller.fill(b.robotPos, b)
        m += (coordinates -- coords).foldLeft(0) { (sum, coord) =>
          points.get(b.get(coord)) match {
            case Some(tileValue) => {
              if (ShortestPathCalculator.dijkstra(b.robotPos, coord, b).isEmpty)
                sum + tileValue
              else
                sum
            }
            case None => sum
          }
        }

        evaluations(b) = m
        m
      }
    }

  }

  private def evaluateState(ops: List[Opcode], b: Board) = {
    evaluateOpcodeList(ops) + evaluateBoard(b)
  }

  private def extractLiftPosition(b: Board) {
    liftPosition = b.tiles.find({ entry => <~(entry._2, 'Lift) }).get._1
  }

  private def extractAllLambdas(b: Board) {
    allLambdas = b.allLambdas
  }

  private def extractCoordinates(b: Board) {
    coordinates = b.tiles.foldLeft(Set[Coordinate]()) { (set, entry) =>
      set + entry._1
    }
  }

  private def evaluateScore(b: Board) = {
    var score = b.lambdas * 25
    if (b.status == Win())
      score += b.lambdas * 50
    else
      0
    score -= b.tick
    score
  }

  private implicit def StateOrdering =
    new Ordering[(String, Int)] {
      def compare(
        a: (String, Int),
        b: (String, Int)) = b._2 - a._2
    }

  val possibleMoves = List('MoveUp, 'MoveDown, 'MoveLeft, 'MoveRight, 'Wait)

  def evaluateBestSolution(b: Board) = {
    extractLiftPosition(b)
    extractAllLambdas(b)
    extractCoordinates(b)

    val visitedStates = MutableMap[String, (List[Opcode], Board)]()
    val pq = PriorityQueue[(String, Int)]()
    val boardEvaluations = MutableMap[String, Int]()
    var bestSoFar = List[Opcode]()
    var bestScore = evaluateScore(b)
    pq += ((b, evaluateState(bestSoFar, b)))
    visitedStates(b) = (bestSoFar -> b)
    boardEvaluations(b) = evaluateState(bestSoFar, b)
    bestInCoord(b.robotPos) = boardEvaluations(b)

    val startTime = System.nanoTime()

    while (!pq.isEmpty &&
           visitedStates(pq.head._1)._2.status != Win() &&
           (System.nanoTime() - startTime) / 1000000 < 100000) {
      val t = pq.dequeue()
      val c = t._1
      val (ops, b) = visitedStates(c)
      val dd = boardEvaluations(c)

      val currentBestInCoord = bestInCoord(b.robotPos)

      if (dd == t._2 && dd <= currentBestInCoord) {
        possibleMoves.foreach { m =>
          val neb = b.eval(m)
          val cd = evaluateState(m :: ops, neb)

          if (evaluateScore(neb) > bestScore) {
            bestSoFar = m :: ops
            bestScore = evaluateScore(neb)
            println("Best so far (" + bestScore + ") = " + Opcode.toString(bestSoFar))
          }

          neb match {
            case rb if rb.status == Playing() | rb.status == Win() => {
              boardEvaluations.get(neb) match {
                case Some(d) if d > cd => {
                  visitedStates(neb) = (m :: ops) -> neb
                  boardEvaluations(neb) = cd
                  pq += ((neb, cd))
                  bestInCoord(neb.robotPos) = cd
                }
                case None => {
                  visitedStates(neb) = (m :: ops) -> neb
                  boardEvaluations(neb) = cd
                  pq += ((neb, cd))
                  bestInCoord(neb.robotPos) = cd
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
