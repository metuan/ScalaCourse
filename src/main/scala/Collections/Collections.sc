import scala.collection.immutable
object Collections {
  val xs = Array(1,2,3,44)
  val ys = "Hello world"
  val data = List(2,3,5,7)


  xs map (x => x * 2)
  ys filter(_.isUpper)
  (data foldLeft 0) (_ + _)
  data.product
  data.min
  data.max

  val r: Range= 1 until 5
  r.start
  r.tail.tail.start

  r.exists(_ == 4)
  r forall(_ % 5 < 5)

  val pairs: List[(Int, Char)] = List(1,2,3) zip "abc"
  pairs unzip

  ys flatMap(c => List('.', c))

  val data3 = List(0,1,2,3,4)
  data3 flatMap(x => data3 map (y => (x,y)))

  data3 flatMap(x => data3 map(y => if (x != y) (x,y)))

  def isPrime(n: Int): Boolean = (2 until n) forall(d => n % d != 0)
  isPrime(7)
  isPrime(144)

  val n = 7
  (1 until n).flatMap(i =>
    (1 until i) map (j =>
      (i, j))) filter (pair => isPrime(pair._1+pair._2))

  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)


  def scalarProduct(xs: List[Double], ys: List[Double]) : Double =
    (for ((x, y) <- xs zip ys) yield x * y).sum
  scalarProduct(List(1,2,3), List(1,2,3))


  val fruits = List("apple", "pear", "banana", "orange", "pineapple")
  fruits groupBy(_.length)
  fruits sortWith(_.length < _.length)
  fruits.sorted
  fruits sortWith(_.head < _.head)


  def makeWhatEverYouLike(xs: List[String], sideEffect: String ⇒ String): List[String] =
    xs map sideEffect

  val myName: (String) => String = (name: String) => s"My name is $name"
  makeWhatEverYouLike(List("John", "Mark"), myName)
  List("Scala", "Erlang", "Clojure") map (_.length)


  val a: List[Int] = (1 to 5).toList

  val myMap: Map[String, String] = {
    Map("MI" → "Michigan", "OH" → "Ohio", "WI" → "Wisconsin", "IA" → "Iowa")
  }
  myMap.getOrElse("TX", "missing data")

  val c = 'a'//unicode for a
  val d = '\141' //octal for a
  val e = '\"'
  val f = '\\'


  case class Dog(name: String, breed: String)
  val d1 = Dog("Scooby", "Doberman")
  d1.toString


  val doubleEvens: PartialFunction[Int, Int] = {
    case x if (x % 2) == 0 ⇒ x * 2
  }
  val tripleOdds: PartialFunction[Int, Int] = {
    case x if (x % 2) != 0 ⇒ x * 3
  }

  val printEven: PartialFunction[Int, String] = {
    case x if (x % 2) == 0 ⇒ "Even"
  }
  val printOdd: PartialFunction[Int, String] = {
    case x if (x % 2) != 0 ⇒ "Odd"
  }

  val whatToDo: PartialFunction[Int, String] = doubleEvens orElse tripleOdds andThen (printEven orElse printOdd)

  whatToDo(3)
  whatToDo(4)

  def from(n: Int): Stream[Int] = n #:: from(n + 1)
  val naturalNumbers: Stream[Int] = from(0)
  val m4s: Stream[Int] = naturalNumbers map(_ * 4)
  naturalNumbers.take(10).toList

  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter(_ % s.head != 0))

  val primeNumbers: Stream[Int] = sieve(from(2))
  primeNumbers.take(100).toList


  def sqrtStream(x: Double): Stream[Double] = {
    def improve(guess: Double) = (guess + x / guess) / 2
    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
  }

  sqrtStream(4)(10)
}
