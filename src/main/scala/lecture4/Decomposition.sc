trait Expr {
  def isNumber: Boolean
  def isSum: Boolean
  def isProd: Boolean
  def numValue: Int
  def leftOp: Expr
  def rightOp: Expr
}

trait Expr1 {
  def eval: Int
}

class Number(n: Int) extends Expr {
  def isNumber: Boolean = true
  def isSum: Boolean = false
  def isProd: Boolean = false
  def numValue: Int = n
  def leftOp: Expr = throw new Error("Number.leftOp")
  def rightOp: Expr = throw new Error("Number.rightOp")
}

class Number1(n:Int) extends Expr1 {
  def eval: Int = n
}

class Sum(expr1: Expr, expr2: Expr) extends Expr {
  def isNumber: Boolean = false
  def isSum: Boolean = true
  def isProd: Boolean = false
  def numValue: Int = throw new Error("Sum.numValue")
  def leftOp: Expr = expr1
  def rightOp: Expr = expr2
}

class Sum1(expr1: Expr1, expr2: Expr1) extends Expr1 {
  def eval: Int = expr1.eval + expr2.eval
}

class Prod(expr1: Expr, expr2: Expr) extends Expr {
  def isNumber: Boolean = false
  def isSum: Boolean = false
  def isProd: Boolean = true
  def numValue: Int = throw new Error("Prod.numValue")
  def leftOp: Expr = expr1
  def rightOp: Expr = expr2
}

class Prod1(expr1: Expr1, expr2: Expr1) extends Expr1 {
  def eval: Int = expr1.eval * expr2.eval
}

def eval(e: Expr): Int = {
  if (e.isNumber) e.numValue
  else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
  else if (e.isProd) eval(e.leftOp) * eval(e.rightOp)
  else throw new Error(s"Unknown expression $e")
}

eval(new Sum(new Number(1), new Number(2)))

new Prod1(new Number1(2), new Number1(3)).eval