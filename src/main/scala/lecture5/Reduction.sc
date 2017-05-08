def sum(xs: List[Int]): Int = xs match {
  case Nil => 0
  case y :: ys => y + sum(ys)
}
val numbers = List(2, 4, 5, 7, 1)
sum(numbers)

def sum2(xs: List[Int]): Int = xs reduceLeft(_ + _)
sum2(numbers)
numbers.sum

def sum3(xs: List[Int]): Int = (xs foldLeft 0)(_ + _)
sum3(numbers)

numbers.product

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())(f(_) :: _)
mapFun[Int, Int](numbers, x => x + 1 )

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (_, y) => 1 + y )
lengthFun(numbers)