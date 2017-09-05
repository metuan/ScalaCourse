package NQueens

object test extends App {

  val q = NQueens
  print("Enter size of N x N chess board: ")
  val n = scala.io.StdIn.readLine().toInt
  val queens: Set[List[Int]] = q.queens(n)
  val size: Int = queens.size
  println("Number of solutions: " + queens.size)
  println("Check if this is correct: " + (size  == q.mapOfCorrectNumberOfSolutions(n)))
  println(if (n >= 5 ) "Only 4 among of " + size + " solutions" else "SOLUTIONS")
  println((queens take 5 map q.show) mkString "\n")

}
