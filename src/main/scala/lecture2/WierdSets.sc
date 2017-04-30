type Set = Int => Boolean
def contains(s: Set, elem: Int): Boolean = s(elem)
def singletonSet(elem: Int): Set = set => set == elem
def toString(s: Set): String = {
  val xs = for (i <- -1000 to 1000 if s(i)) yield i
  xs.mkString("{", ",", "}")
}

val s1 = singletonSet(1)
toString(s1)
val s2 = singletonSet(1)
toString(s2)

def union(s: Set, t: Set): Set = i => s(i) || t(i)
toString(union(s1, s2))

def intersect(s: Set, t: Set): Set = i => s(i) && t(i)
toString(intersect(s1, s2))

def diff(s: Set, t: Set): Set = i => s(i) != t(i)
toString(diff(s1, s2))


val s5: Set = union(singletonSet(1), singletonSet(201))
val s6: Set = union(singletonSet(2), singletonSet(4))
val s7: Set = union(s5, s6)
toString(s7)
def filter(s: Set, p: Int => Boolean): Set = i => s(i) && p(i)
toString(filter(s7, x => x % 2 == 0))

val bound = 1000
def forall(s: Set, p: Int => Boolean): Boolean = {
  def iterate(a: Int): Boolean = {
    if (a < -bound) true
    else if (s(a) && !p(a)) false
    else iterate(a - 1)
  }
  iterate(bound)
}
val s3 = singletonSet(1)
forall(s3, x => x % 2 == 0)
!forall(s3, x => x % 2 == 0)

def exists(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a < -bound) false
    else if (s(a) && p(a)) true
    else iter(a - 1)
  }
  iter(bound)
}
val s4 = singletonSet(2)
exists(s4, x => x % 2 == 0)

val s8: Set = union(singletonSet(1), singletonSet(201))
val s9: Set = union(singletonSet(3), singletonSet(5))
val s10: Set = union(s8, s9)
toString(s10)
def exists2(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))
exists2(s10, x => x % 2 == 0)
exists2(union(s4, s10), x => x % 2 == 0)

def map(s: Set, f: Int => Int): Set = i => exists(s, j => f(j) == i)
toString(map(s7, x => x * 2))