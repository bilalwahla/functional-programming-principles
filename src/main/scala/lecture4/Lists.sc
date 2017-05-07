val fruits = List("Apples", "Oranges", "Mangoes")
val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))

fruits.tail.tail.head

val list = List(7, 3, 9, 2)

def insertionSort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, insertionSort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
}

insertionSort(list)