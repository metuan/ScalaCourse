

trait MutablePair {
  type A
  type B

  val initA: A
  val initB: B

  private var _fst: A = initA
  private var _snd: B = initB

  def fst: A = _fst
  def snd: B = _snd

  def fst_=(x: A) = _fst = x
  def snd_=(x: B) = _snd = x

  override def toString(): String = {
    "(" + fst + ", " + snd + ")"
  }
}

trait Abstract {
  type T
  def transform(x: T): T
  val initial: T
  var current = initial
  override def toString(): String = {
    current + ""
  }
}

abstract class AbsCell {
  type T
  val init: T
  private var value = init
  def get: T = value
  def set(x: T) = value = x
}

object Zadanie2Test {
  def main(args: Array[String]): Unit = {
    val cell = new AbsCell { type T = Int; val init = 1 }
    println(cell.get)
    //    val c = new Abstract { type T = String; def transform(x: T) = x + x; val initial = "hi! " }
    //    c.current = "hello"
    //    println(c.current)
    //
    var mt = new MutablePair {
      type A = Int
      type B = String
      val initA = 2
      val initB = "Napis"
    }
    mt.fst = 5
    println(mt.fst)
  }
}