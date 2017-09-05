package Generators

object test extends App {
  val g = new Generators
  println(g.randPair(g.randBoolean, g.randInteger).generate)
  println(g.randTree.generate)
  g.test(g.randPair(g.lists, g.lists)) {
    case (xs, ys) => (xs ++ ys).length >= xs.length
  }
}
