package Polynomials


object test extends App {
  val p1 = new Polynomials(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
  val p2 = new Polynomials(0 -> 3.0, 3 -> 7.0)
  println(p1)
  println(p2)
  println(p1 + p2)
}
