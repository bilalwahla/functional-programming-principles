def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)
isPrime(6)
isPrime(7)

val n = 7
(1 until n) flatMap (i =>
  (1 until i) map (j => (i, j))) filter (pair => isPrime(pair._1 + pair._2))

// Above is not very easy to understand. 'For' expression can help
//case class Person(name: String, age: Int)

/*
To obtain the names of the people aged over 20, we can write:
for (p <- persons if p.age > 20) yield p.name

Which is equivalent to:
persons filter(p => p.age > 20) map (p => p.name)
*/

/*
In the for expression above we can use for { } instead of for () allowing us
to write sequences and generators in multiple lines emitting the semi colons.
 */

// SO
for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

// And
def scalarProduct(xs: Vector[Double], ys: Vector[Double]) =
  (xs zip ys) map(xy => xy._1 * xy._2) sum

// Can also be written as
def scalarProduct2(xs: Vector[Double], ys: Vector[Double]) =
  (for ((x, y) <- xs zip ys) yield x * y) sum

val v1 = Vector(2.0, 3, 4)
val v2 = Vector(5.0, 6, 7)
scalarProduct(v1, v2)
scalarProduct2(v1, v2)