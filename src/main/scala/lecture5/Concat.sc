def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case x :: xs1 => x :: concat(xs1, ys)
}
val xs = List("abc", "ijk", "xyz", "Bilal")
val ys = List("lmn", "pqr", "Wahla")
concat(xs, ys)
concat(xs, Nil)

val x = "xxx"
val y = "yyy"

x :: xs
(x :: xs).reverse
// above is same as
xs.reverse ++ List(x)

xs.reverse.reverse // is same as xs

(x :: xs).reverse.reverse
// above is same as
(xs.reverse ++ List(x)).reverse
// which is same as
x :: xs.reverse.reverse

((y :: ys) ++ List(x)).reverse
// above is equal to below by 2nd clause of ++
(y :: (ys ++ List(x))).reverse
// which is same as below by 2nd clause of reverse
(ys ++ List(x)).reverse ++ List(y)
// which is same as below by induction hypothesis
(x :: ys.reverse) ++ List(y)
// and which by 2nd clause of reverse is same as
x :: (y :: ys).reverse

def f = (s: String) => s.contains("la")
xs ++ ys map f equals (xs map f) ++ (ys map f)
Nil map f equals Nil
(x :: xs) map f equals f(x) :: (xs map f)