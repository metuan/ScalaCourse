

trait BinaryHeap[A] {
  def insert(value: A): Unit
  def top(): A
  def pop(): Unit
  def check(value: A): Boolean
  def size(): Int
  def isEmpty(): Boolean
  def get(index: Int): A
}