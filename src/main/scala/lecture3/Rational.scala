package lecture3

/**
  * .
  *
  * @author bilalwahla
  */
class Rational(x: Int, y: Int) {
  require(y != 0, "Not a rational number")

  // Secondary constructors
  def this(x: Int) = this(x, 1)

  def numerator: Int = x
  def denominator: Int = y

  def + (r: Rational) = new Rational(
    numerator * r.denominator + r.numerator * denominator,
    denominator * r.denominator
  )

  def - (r: Rational): Rational = this + -r // DRY

  def * (r: Rational) = new Rational(
    numerator * r.numerator, denominator * r.denominator
  )

  def / (r: Rational) = new Rational(
    numerator * r.denominator, denominator * r.numerator
  )

  def unary_- = new Rational(-numerator, denominator)

  def < (r: Rational): Boolean = numerator * r.denominator < r.numerator * denominator

  def max(r: Rational): Rational = if (this < r) r else this

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  override def toString: String = {
    val g = gcd(numerator, denominator)
    numerator / g + "/" + denominator / g
  }
}