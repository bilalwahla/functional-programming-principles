val fruits = List("Apples", "Oranges", "Mangoes", "Pears", "Grapes")
val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))

fruits.head
fruits.tail
fruits.length
fruits.last
fruits.init
fruits.take(3) // take first 3
fruits.drop(2) // drop first 2
fruits(2)
fruits.reverse
fruits ++ diag3
fruits.updated(0, "Banana")
fruits.indexOf("Mangoes")
fruits.contains("Pears")

def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys => last(ys)
}
last(fruits)

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}
init(fruits)

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}
concat(fruits, diag3)

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}
reverse(fruits)

def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
  case List() => throw new Error(s"remove at $n from empty list")
  case List(x) if n != 0 => throw new Error("index out of bound")
  case List(x) => List()
  case y :: ys if n == 0 => ys
  case y :: ys => y :: removeAt(n - 1, ys)
}
removeAt(0, List("abc"))
removeAt(1, List('a', 'b', 'c', 'd')) // List(a, c, d)
removeAt(0, fruits)
removeAt(2, fruits)
removeAt(3, fruits)

def removeAt2[T](n: Int, xs: List[T]): List[T] =
  (xs take n) ::: (xs drop n + 1)
removeAt2(1, List('a', 'b', 'c', 'd')) // List(a, c, d)