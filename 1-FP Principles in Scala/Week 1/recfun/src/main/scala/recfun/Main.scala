package recfun

import scala.collection.mutable.{ListBuffer}

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == r || c == 0) 1
      else pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanced(chars: List[Char], openedParentheses: Int): Boolean =
        if (chars.isEmpty) openedParentheses == 0
        else if (chars.head == '(') balanced(chars.tail,openedParentheses + 1)
        else if (chars.head == ')') openedParentheses > 0 && balanced(chars.tail,openedParentheses - 1)
        else balanced(chars.tail,openedParentheses)
      balanced(chars,0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChangeAux(lastMaxCoinTotalColl: List[(Int, Int)], count: Int): Int = {
        if (lastMaxCoinTotalColl.isEmpty) count
        else {
          val buffer = ListBuffer[(Int, Int)]()
          var newCount = count
          for ((lastMaxCoin, total) <- lastMaxCoinTotalColl) {
            if (total < money) {
              for (coin <- coins) {
                if (coin >= lastMaxCoin) {
                  val num = (coin, total + coin)
                  buffer += num
                }
              }
            } else if (total == money) newCount += 1
          }
          countChangeAux(buffer.toList, newCount)
        }
      }
      val buffer = coins.map { coin => (coin, coin) }
      countChangeAux(buffer, 0)
    }
}
