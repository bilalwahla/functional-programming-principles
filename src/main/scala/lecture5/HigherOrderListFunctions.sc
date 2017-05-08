def scaleList(xs: List[Double], factor: Double): List[Double] = xs match {
  case Nil => xs
  case y :: ys => y * factor :: scaleList(ys, factor)
}
val doubles = List(2.0, 4, 5, 7, 1)
scaleList(doubles, 2)
doubles.map(x => x * 2)

def scaleList2(xs: List[Double], factor: Double): List[Double] =
  xs map(x => x * factor)
scaleList2(doubles, 3)

def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => y * y :: squareList(ys)
}
val numbers = List(2, -4, 5, -7, 1)
squareList(numbers)

def squareList2(xs: List[Int]): List[Int] = xs map(x => x * x)
squareList2(numbers)

def posElems(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => if (y > 0) y :: posElems(ys) else posElems(ys)
}
posElems(numbers)

def posElems2(xs: List[Int]): List[Int] = xs filter(x => x > 0)
posElems2(numbers)

// Other list functions
numbers filter(x => x > 0)
numbers filterNot(x => x > 0)
val (pos, neg) = numbers partition(x => x > 0)
numbers takeWhile(x => x > 0)
numbers dropWhile(x => x > 0)
val (firstPositives, rest) = numbers span(x => x > 0)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case y :: ys =>
    val (firstConsecutive, rest) = xs span(x => x == y)
    firstConsecutive :: pack(rest)
}
pack(List("a", "a", "a", "b", "c", "c", "a"))

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs).map(ys => (ys.head, ys.length))
encode(List("a", "a", "a", "b", "c", "c", "a"))