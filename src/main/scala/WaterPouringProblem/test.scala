package WaterPouringProblem

object test extends App {
  val problem = new WaterPouringProblem(Vector(4,7))
  println(problem.solution(6).take(1).toList)
}
