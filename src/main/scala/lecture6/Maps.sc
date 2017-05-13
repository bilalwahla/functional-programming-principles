val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
val capitalOfCountry = Map("US" -> "Washington", "Pakistan" -> "Islamabad")

capitalOfCountry("Pakistan")
//capitalOfCountry("England")

capitalOfCountry get "England"
capitalOfCountry get "Pakistan"

def showCapital(country: String) = capitalOfCountry.get(country) match {
  case Some(capital) => capital
  case None => "missing data"
}

showCapital("Pakistan")
showCapital("England")

val cap = capitalOfCountry withDefaultValue "Unknown"
cap("England")

val fruit = List("Pineapple", "Apple", "Banana", "Mango", "Pear")
fruit sortWith(_.length < _.length)
fruit sorted

fruit groupBy(_.head)

class Poly(terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms: Map[Int, Double] = terms0 withDefaultValue 0.0

  def +(other: Poly) = new Poly(terms ++ (other.terms map adjust))

  def plus(other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coefficient) = term
    terms + (exp -> (coefficient + terms(exp)))
  }

  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coefficient) = term
//    terms get exp match {
//      case Some(coefficient1) => exp -> (coefficient + coefficient1)
//      case None => exp -> coefficient
//    }
    // With terms with a default value we can replace above code like so
    exp -> (coefficient + terms(exp))
  }

  override def toString: String =
    (for ((exp, coefficient) <- terms.toList.sorted.reverse)
      yield coefficient + "x^" + exp) mkString " + "
}

val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = new Poly(0 -> 3, 3 -> 7.0)
p1 + p2
p1 plus p2

p1.terms(7)