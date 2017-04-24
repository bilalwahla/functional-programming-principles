def sum(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f)(a + 1, b)
sum(x => x * x)(3, 5)

def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1 else f(a) * product(f)(a + 1, b)
product(x => x * x)(3, 4)

def fact(n: Int) = product(x => x)(1, n)
fact(3)

// Generalise sum and product
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
}

def product2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)
product(x => x * x)(3, 4)

def sum2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x + y, 0)(a, b)
sum2(x => x * x)(3, 5)

import math.abs
val tolerance = 0.0001
def isCloseEnough(x: Double, y: Double) = abs((x - y) / x) / x < tolerance
def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    println(s"Guess: $guess")
    val next = f(guess)
    if (isCloseEnough(guess, next)) next else iterate(next)
  }
  iterate(firstGuess)
}
fixedPoint(x => 1 + x/2)(1)

def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2)(1.0)
sqrt(2)

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def sqrt2(x: Double) = fixedPoint(averageDamp(y => x / y))(1.0)
sqrt2(2)