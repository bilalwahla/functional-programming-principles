def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, yss) => yss
      case (xss, Nil) => xss
      case (x :: xss, y :: yss) =>
        if (lt(x, y)) x :: merge(xss, ys) else y :: merge(xs, yss)
    }
    val (first, second) = xs splitAt n
    merge(msort(first)(lt), msort(second)(lt))
  }
}
val numbers = List(2, -4, 5, 7, 1)
msort(numbers)((x, y) => x < y)
val fruits = List("Apples", "Oranges", "Mangoes", "Pears", "Grapes")
msort(fruits)((s1, s2) => s1.compareTo(s2) < 0)

def msort2[T](xs: List[T])(ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, yss) => yss
      case (xss, Nil) => xss
      case (x :: xss, y :: yss) =>
        if (ord.lt(x, y)) x :: merge(xss, ys) else y :: merge(xs, yss)
    }
    val (first, second) = xs splitAt n
    merge(msort2(first)(ord), msort2(second)(ord))
  }
}
msort2(numbers)(Ordering[Int])
msort2(fruits)(Ordering[String])

def msort3[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, yss) => yss
      case (xss, Nil) => xss
      case (x :: xss, y :: yss) =>
        if (ord.lt(x, y)) x :: merge(xss, ys) else y :: merge(xs, yss)
    }
    val (first, second) = xs splitAt n
    merge(msort3(first), msort3(second))
  }
}
msort3(numbers)
msort3(fruits)