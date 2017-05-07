def msort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
      case (Nil, yss) => yss
      case (xss, Nil) => xss
      case (x :: xss, y :: yss) =>
        if (x < y) x :: merge(xss, ys) else y :: merge(xs, yss)
    }
    val (first, second) = xs splitAt n
    merge(msort(first), msort(second))
  }
}

val numbers = List(2, -4, 5, 7, 1)
msort(numbers)