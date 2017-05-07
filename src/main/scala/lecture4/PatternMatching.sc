trait Expr1 {
  def eval: Int = this match {
    case Number1(n) => n
    case Sum1(e1, e2) => e1.eval + e2.eval
    case Prod1(e1, e2) => e1.eval * e2.eval
  }
}

case class Number1(n:Int) extends Expr1

case class Sum1(expr1: Expr1, expr2: Expr1) extends Expr1

case class Prod1(expr1: Expr1, expr2: Expr1) extends Expr1

Sum1(Number1(2), Number1(3)).eval
Prod1(Number1(2), Number1(3)).eval
Sum1(Sum1(Number1(2), Number1(3)), Prod1(Number1(2), Number1(3))).eval

def show(e: Expr1): String = e match {
  case Number1(n) => n.toString
  case Sum1(expr1, expr2) => show(expr1) + " + " + show(expr2)
  case Prod1(expr1, expr2) => show(expr1) + " * " + show(expr2)
}

show(Number1(2))
show(Sum1(Number1(2), Number1(3)))
show(Prod1(Number1(2), Number1(3)))
show(Sum1(Sum1(Number1(2), Number1(3)), Prod1(Number1(2), Number1(3))))