package lecture3

/**
  * Hello Scala World.
  *
  * @author bilalwahla
  */
object Hello {
  def main(args: Array[String]): Unit = {
    println("Hello Scala World")

    val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
    println(nth(2, list))

    val tree1 = new NonEmpty(2, Empty, Empty)
    val tree2 = new NonEmpty(1, Empty, Empty)
    val tree3 = new NonEmpty(16, Empty, Empty) union tree1 union tree2 incl 5 union new NonEmpty(8, Empty, Empty) union
      new NonEmpty(0, Empty, Empty)
    println(tree3)
    println(tree3.filterAccumulator(x => x %2 == 0, Empty))
    println(tree3.max)
    println(tree3.remove(tree3.max))
    println(new Cons(tree3.max, new Nil).head)
    val orderedList: List[Int] = tree3.descendingByInt
    orderedList foreach println
  }

  def nth[T](i: Int, list: List[T]): T =
    if (i == 0) list.head else nth(i - 1, list.tail)
}
