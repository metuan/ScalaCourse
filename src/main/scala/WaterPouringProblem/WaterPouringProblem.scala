package WaterPouringProblem

import scala.collection.immutable

class WaterPouringProblem(capacity: Vector[Int]) {

  // States
  type State = Vector[Int]
  val initialState: Vector[Int] = capacity map (x => 0)

  // Moves
  trait Move { def change(state: State): State }
  case class Empty(glass: Int) extends Move {
    def change(state: State): State = state updated (glass, 0)
    override def toString: String = "Emptied glass " + glass + "\n"
  }
  case class Fill(glass: Int) extends Move {
    def change(state: State): State = {
      state updated(glass, capacity(glass))
    }
    override def toString: String = "Filled glass " + glass + " \n"
  }
  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State): State = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated(from, state(from) - amount) updated (to, state(to) + amount)
    }
    override def toString: String = "Poured from glass " + from + " to " + to + "\n"
  }
  val glasses: Range = capacity.indices
  val moves: immutable.IndexedSeq[Move with Product with Serializable] =
    (for (g <- glasses) yield Empty(g)) ++
    (for (g <- glasses) yield Fill(g)) ++
    (for (from <- glasses; to <- glasses if from != to) yield  Pour(from, to))

  // Paths
  class Path(history: List[Move], val endState: State) {
    def extend(move: Move) = new Path(move :: history, move change endState)
    override def toString: String = (history.reverse mkString " -- ") + "--> " + endState
  }

  val initialPath: Path = new Path(Nil, initialState)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  val pathSets: Stream[Set[Path]] = from(Set(initialPath), Set(initialState))

  def solution(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
}
