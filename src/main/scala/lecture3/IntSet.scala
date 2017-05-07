package lecture3

/**
  * .
  *
  * @author bilalwahla
  */
abstract class IntSet {
  def incl(x: Int): IntSet

  def remove(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(other: IntSet): IntSet

  def filterAccumulator(p: Int => Boolean, accumulated: IntSet): IntSet

  def max: Int

  def ascendingByInt: List[Int] = {
    def order(subset: IntSet, orderedList: List[Int]): List[Int] = {
      val maxInt = subset.max
      val remainderSet = subset.remove(maxInt)
      if (subset.contains(maxInt)) order(remainderSet, new Cons(maxInt, orderedList)) else orderedList
    }

    order(this, new Nil)
  }
}

object Empty extends IntSet {
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  def remove(x: Int): IntSet = this

  def contains(x: Int): Boolean = false

  override def toString: String = "."

  def union(other: IntSet): IntSet = other

  def filterAccumulator(p: Int => Boolean, accumulated: IntSet): IntSet = accumulated

  def max: Int = 0
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  def remove(x: Int): IntSet = if (x < elem) new NonEmpty(elem, left.remove(x), right)
  else if (elem < x) new NonEmpty(elem, left, right.remove(x))
  else left.union(right)

  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  override def toString: String = "{" + left + elem + right + "}"

  def union(other: IntSet): IntSet = ((left union right) union other) incl elem

  def filterAccumulator(p: Int => Boolean, accumulated: IntSet): IntSet = {
    def accumulate(a: IntSet): IntSet = if (p(elem)) a.incl(elem) else a
    accumulate(left.filterAccumulator(p, right.filterAccumulator(p, accumulated)))
  }

  def max: Int = {
    val maxOnTheLeft = left.max; val maxOnTheRight = right.max
    if (elem > maxOnTheLeft && elem > maxOnTheRight) elem
    else if (maxOnTheLeft > maxOnTheRight) maxOnTheLeft
    else maxOnTheRight
  }
}