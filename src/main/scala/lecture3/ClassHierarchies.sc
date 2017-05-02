import lecture3.{Cons, Nil}

abstract class IntSet {
  def incl(x: Int): IntSet

  def remove(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(other: IntSet): IntSet

  def filterAccumulator(p: Int => Boolean, accumulated: IntSet): IntSet

  def max: Int
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
    val maxOnTheLeft = left.max
    val maxOnTheRight = right.max
    if (elem > maxOnTheLeft && elem > maxOnTheRight) elem
    else if (maxOnTheLeft > maxOnTheRight) maxOnTheLeft
    else maxOnTheRight
  }
}

val tree1 = new NonEmpty(2, Empty, Empty)
val tree2 = new NonEmpty(1, Empty, Empty)
val tree3 = new NonEmpty(16, Empty, Empty) union
  tree1 union
  tree2 incl
  5 union
  new NonEmpty(8, Empty, Empty) union
  new NonEmpty(0, Empty, Empty)
tree3.filterAccumulator(x => x %2 == 0, Empty)
tree3
tree3.max
tree3.remove(tree3.max)
tree3
new Cons(tree3.max, new Nil).head