

class EmptyException(message: String = "") extends RuntimeException(message)

//Viewbounds <% are deprecated, but they enable implicit coercions from Int to RichInt.
//RichInt is a subclass of Int which is a subtype of Ordered thus allowing comparisons.
class BinaryHeapImpl[A <% Ordered[A]: Manifest] extends BinaryHeap[A] {
  private def giveArray[A: Manifest](size: Int): Array[A] = {
    new Array[A](size)
  }
  private var array: Array[A] = giveArray[A](10)
  private var _size = 0

  override def insert(value: A): Unit = {
    if (size >= array.length - 1)
      grow()
    _size += 1
    var index: Int = size
    array(index) = value
    bubbleUp()
  }

  override def top(): A = {
    if (size <= 0)
      throw new EmptyException("Heap empty.")
    return array(1)
  }

  override def pop(): Unit = {
    if (!isEmpty()) {
      array(1) = array(size);
      _size -= 1;
      bubbleDown();
    }
  }

  override def check(value: A): Boolean = {
    var i: Int = 1
    while (i <= size) {
      if (array(i) == value)
        return true
      i += 1
    }
    return false
  }

  override def size: Int = _size

  override def isEmpty(): Boolean = size == 0

  override def toString(): String = {
    var sb: StringBuilder = new StringBuilder("[")
    var i: Int = 1
    while (i <= size) {
      sb.append(array(i)).append(", ")
      i += 1
    }
    if (sb.length > 2)
      sb.setLength(sb.length - 2)
    sb.append("]")
    sb.toString()
  }

  override def get(index: Int): A = {
    array(index + 1)
  }

  private def bubbleDown(): Unit = {
    var index: Int = 1;
    var break: Boolean = false
    while (hasLeftChild(index) && !break) {
      var biggerChild: Int = leftIndex(index)
      if (hasRightChild(index) && (array(leftIndex(index)) < array(rightIndex(index))))
        biggerChild = rightIndex(index);
      if (array(index) < array(biggerChild))
        swap(index, biggerChild);
      else
        break = true
      index = biggerChild;
    }
  }

  private def bubbleUp(): Unit = {
    var index: Int = size;
    while (hasParent(index) && (parent(index) < array(index))) {
      swap(index, parentIndex(index))
      index = parentIndex(index)
    }
  }

  private def hasParent(i: Int): Boolean = {
    i > 1;
  }

  private def leftIndex(i: Int): Int = {
    i * 2
  }

  private def rightIndex(i: Int): Int = {
    i * 2 + 1
  }

  private def hasLeftChild(i: Int): Boolean = {
    leftIndex(i) <= size
  }

  private def hasRightChild(i: Int): Boolean = {
    rightIndex(i) <= size
  }

  private def parent(i: Int): A = {
    array(parentIndex(i))
  }

  private def parentIndex(i: Int): Int = {
    i / 2
  }

  private def grow(): Unit = {
    var newArray: Array[A] = giveArray[A](array.length * 2)
    Array.copy(array, 0, newArray, 0, array.length)
    array = newArray
  }

  private def swap(i1: Int, i2: Int): Unit = {
    var temp: A = array(i1);
    array(i1) = array(i2);
    array(i2) = temp;
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    var q = new BinaryHeapImpl[Int]()
    q.insert(10);
    q.insert(3);
    q.insert(5);
    q.insert(2);
    q.insert(0);
    q.insert(8);
    println(q);
    while (!q.isEmpty()) {
      print(q.top());
      print(", ");
      q.pop();
    }
  }
}