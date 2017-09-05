package NQueens

object NQueens extends App {

  val mapOfCorrectNumberOfSolutions = Map(1 -> 1, 2 -> 0, 3 -> 0, 4 -> 2, 5 -> 10, 6 -> 4, 7 -> 40, 8 -> 92, 9 -> 352, 10 -> 724)


  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] =
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k-1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
    placeQueens(n)
  }

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    queensWithRow forall {
      case (r,c) => col != c && math.abs(col - c) != row - r
    }
  }

  def show(queens: List[Int]): String = {
    val lines =
      for (col <- queens.reverse)
        yield Vector.fill(queens.length)("* ")
          .updated(col, "Q ")
          .mkString
    "\n" + (lines mkString "\n")
  }

}

