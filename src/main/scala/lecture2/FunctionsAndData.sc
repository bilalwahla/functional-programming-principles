class Rational(x: Int, y: Int) {
  require(y != 0, "Not a rational number")

  // Secondary constructors
  def this(x: Int) = this(x, 1)

  def numerator = x
  def denominator = y

  def add(r: Rational) = new Rational(
    numerator * r.denominator + r.numerator * denominator,
    denominator * r.denominator
  )

  def subtract(r: Rational) = new Rational(
    numerator * r.denominator - r.numerator * denominator,
    denominator * r.denominator
  )

  def subtract2(r: Rational) = add(r.neg) // DRY

  def multiply(r: Rational) = new Rational(
    numerator * r.numerator, denominator * r.denominator
  )

  def divide(r: Rational) = new Rational(
    numerator * r.denominator, denominator * r.numerator
  )

  def neg = new Rational(-numerator, denominator)

  def less(r: Rational) = numerator * r.denominator < r.numerator * denominator

  def max(r: Rational) = if (this.less(r)) r else this

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  override def toString: String = {
    val g = gcd(numerator, denominator)
    numerator / g + "/" + denominator / g
  }
}

val x = new Rational(1, 3)
x.numerator
x.denominator

val y = new Rational(5, 7)
x.add(y)
y.add(y)

val z = new Rational(3, 2)
x.add(y).multiply(z)

x.subtract(y).subtract(z)
x.subtract2(y).subtract2(z)

x.less(y)
x.max(y)

//val irrational = new Rational(1, 0)