// Arrays
val xs = Array(1, 2, 3, 44)
xs map(x => x * 2)

// Strings
val s = "Bilal Wahla"
s filter(_.isUpper)

// Ranges
val r1: Range = 1 until 5 // 1, 2, 3, 4
val r2: Range = 1 to 5 // 1, 2, 3, 4, 5
1 to 10 by 3 // 1, 4, 7, 10
6 to 1 by -2 // 6, 4, 2

s exists(_.isUpper)
s forall(_.isUpper)

val pair = List(2, 9, 12) zip s
pair unzip

s flatMap(List(_, '.'))

xs.sum
xs.max
xs.product
xs.min

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys) map(xy => xy._1 * xy._2) sum
def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys) map { case (x, y) => x * y } sum
val v1 = Vector(2.0, 3, 4)
val v2 = Vector(5.0, 6, 7)
scalarProduct(v1, v2)
scalarProduct2(v1, v2)
