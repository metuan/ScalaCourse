package Observer

object test extends App {
  val a,b = new BankAccount
  val c = new Consolidator(List(a,b))
  println(c.totalBalance)
  a deposit 20
  println(c.totalBalance)
  b deposit 30
  println(c.totalBalance)
}
