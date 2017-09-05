package Generators

trait Tree
case class Node(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

trait Generator[+T] {
  def generate: T
  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate: S = f(Generator.this.generate)
  }
  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate: S = f(Generator.this.generate).generate
  }
}

class Generators {

  val randInteger = new Generator[Int] {
    def generate: Int = scala.util.Random.nextInt()
  }
  val randBoolean: Generator[Boolean] = randInteger.map (_ > 0)

  def randPair[T, U](t: Generator[T], u: Generator[U]) = new Generator[(T,U)] {
    def generate: (T, U) = (t.generate, u.generate)
  }

  def single[T](x :T): Generator[T] = new Generator[T] {
    def generate: T = x
  }

  def chooseFromRange(lowerBound: Int, upperBound: Int): Generator[Int] =
    for (x <- randInteger) yield lowerBound + x % (upperBound - lowerBound)

  def oneOf[T](xs: T*): Generator[T] =
    for (index <- chooseFromRange(0, xs.length)) yield xs(index)

  def lists: Generator[List[Int]] = for {
    isEmpty <- randBoolean
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list

  def emptyLists: Generator[Nil.type] = single(Nil)

  def nonEmptyLists: Generator[List[Int]] = for {
    head <- randInteger
    tail <- lists
  } yield head :: tail

  def randLeaf: Generator[Leaf] = for {
    x <- randInteger
  } yield Leaf(x)

  def randNode: Generator[Node] = for {
    leftTree <- randTree
    rightTree <- randTree
  } yield Node(leftTree, rightTree)

  def randTree: Generator[Tree] = for {
    isLeaf <- randBoolean
    tree <- if (isLeaf) randLeaf else randNode
  } yield tree

  def test[T](generator: Generator[T], numberOfTests: Int = 100)(test: T => Boolean): Unit = {
    for (i <- 0 until numberOfTests) {
      val value = generator.generate
      assert(test(value), "test failed for " + value)
    }
    println("passed " + numberOfTests + " tests")
  }
}
