package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println("Balanced Parenthesis -- (if (zero? x) max (/ 1 x))")
    print(balance("(if (zero? x) max (/ 1 x))".toList))
    println()
    println("Un-balanced Parenthesis -- ())(")
    print(balance("())(".toList))
    println()
    println("Count Change -- 4 using coins [1, 2]")
    print(countChange(4, List(1, 2)))
    println()
    println("Degenerate Count Change -- 4 using coins [] ")
    print(countChange(4, List[Int]()))
    println()
    println("Degenerate Count Change -- 0 using coins [1] ")
    print(countChange(0, List(1)))
    println()
    println("Degenerate Count Change -- 0 using coins [] ")
    print(countChange(0, List[Int]()))
    println()
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def countParens(chars: List[Char], accumulator: Int): Boolean = {
        if (chars == Nil) accumulator == 0
        else {
          val parens = chars.head match {
            case '(' => accumulator + 1
            case ')' => accumulator - 1
            case _ => accumulator
          }
          if (parens >= 0) countParens(chars.tail, parens) else false
        }
      }
      countParens(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def count(money: Int, coins: List[Int]): Int = {
        if (money == 0) 1
        else if (money < 0 || coins.isEmpty) 0
        else {
          count(money - coins.head, coins) + count(money, coins.tail)
        }
      }
      if (money == 0) 0 else count(money, coins)
    }
  }
