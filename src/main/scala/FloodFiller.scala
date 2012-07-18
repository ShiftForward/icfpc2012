import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import Tile._

object FloodFiller {
  val neighbors = List(Coordinate(1, 0),
                       Coordinate(-1, 0),
                       Coordinate(0, 1),
                       Coordinate(0, -1))
  val floodable = Map(
    ('Invalid -> false),
    ('Robot -> true),
    ('Wall -> false),
    ('Rock -> false),
    ('FallingRock -> false),
    ('Lambda -> true),
    ('ClosedLift -> true),
    ('OpenLift -> true),
    ('Earth -> true),
    ('Empty -> true),
    ('Beard -> false),
    ('HORock -> false),
    ('Razor -> true),
    ('TrampolineA -> true),
    ('TrampolineB -> true),
    ('TrampolineC -> true),
    ('TrampolineD -> true),
    ('TrampolineE -> true),
    ('TrampolineF -> true),
    ('TrampolineG -> true),
    ('TrampolineH -> true),
    ('TrampolineI -> true),
    ('Target1 -> true),
    ('Target2 -> true),
    ('Target3 -> true),
    ('Target4 -> true),
    ('Target5 -> true),
    ('Target6 -> true),
    ('Target7 -> true),
    ('Target8 -> true),
    ('Target9 -> true))

  def fill(start: Coordinate, b: Board): Set[Coordinate] = {
    val visited = Set[Coordinate]()
    val flooded = Set[Coordinate]()
    val q = Queue[Coordinate]()
    q += start
    visited += start

    while (!q.isEmpty) {
      val c = q.dequeue()
      flooded += c

      neighbors.foreach { n =>
        val next = n + c
        if (!visited(next)) {
          visited += next
          if (floodable(b.get(next)) && !flooded.contains(next)) {
            q += next
          }
        }
      }
    }

    flooded
  }
}
