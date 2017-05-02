package lecture3

/**
  * .
  *
  * @author bilalwahla
  */
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def foreach(f: T => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.tail")

  override def toString: String = "."
}