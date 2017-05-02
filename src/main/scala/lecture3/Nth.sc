import lecture3._

def nth[T](i: Int, list: List[T]): T =
  if (list.isEmpty) throw new IndexOutOfBoundsException
  else if (i == 0) list.head
  else nth(i - 1, list.tail)

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
nth(3, list)