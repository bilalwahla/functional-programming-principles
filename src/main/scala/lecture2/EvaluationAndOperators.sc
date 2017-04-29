class Rational(x: Int, y: Int) {
  require(y != 0, "Not a rational number")

  // Secondary constructors
  def this(x: Int) = this(x, 1)

  def numerator = x
  def denominator = y

  def + (r: Rational) = new Rational(
    numerator * r.denominator + r.numerator * denominator,
    denominator * r.denominator
  )

  def - (r: Rational) = this + -r // DRY

  def * (r: Rational) = new Rational(
    numerator * r.numerator, denominator * r.denominator
  )

  def / (r: Rational) = new Rational(
    numerator * r.denominator, denominator * r.numerator
  )

  def unary_- = new Rational(-numerator, denominator)

  def < (r: Rational) = numerator * r.denominator < r.numerator * denominator

  def max(r: Rational) = if (this < r) r else this

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  override def toString: String = {
    val g = gcd(numerator, denominator)
    numerator / g + "/" + denominator / g
  }
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x + y
y + y
x + y * z
x - y - z
x < y
x max y

// Precedence rules
/*
(all letters)
|
^
&
< >
= !
:
+ -
/ * %
(all other special characters)
 */
// a + b ^? c ?^ d less a ==> b | c
// (a + (b ^? (c ?^ d))) less ((a ==> b) | c)