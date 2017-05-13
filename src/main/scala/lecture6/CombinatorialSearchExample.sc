val fruit = Set("Apple", "Banana", "Mango")

val s = (1 to 6).toSet

s map (_ + 2)
fruit filter(_.startsWith("A"))
s nonEmpty

s contains 3
fruit contains "Apple"

def isSafe(col: Int, queens: List[Int]): Boolean = {
  val row = queens.length
  val queensWithRows = (row - 1 to 0 by -1) zip queens
  queensWithRows forall {
    case (r, c) => col != c && math.abs(col - c) != row - r
  }
}

def queens(n: Int): Set[List[Int]] = {
  def placeQueen(q: Int): Set[List[Int]] =
    if (q == 0) Set(List())
    else for {
      queens <- placeQueen(q - 1)
      col <- 0 until n
      if isSafe(col, queens)
    } yield col :: queens
  placeQueen(n)
}

def show(queens: List[Int]) = {
  val lines = for (col <- queens.reverse)
    yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  "\n" + (lines mkString "\n")
}

queens(4) map show mkString "\n"
queens(8) take 3 map show mkString "\n"