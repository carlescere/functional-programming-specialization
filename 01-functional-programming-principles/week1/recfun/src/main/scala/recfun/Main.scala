package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(balance("something".toList))
    println(balance("((())())".toList))
    println(balance("()(())(())".toList))
    println(balance("()(()))(())".toList))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if (c == 0 || r == 0 || c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      @tailrec
      def tailBalance(chars: List[Char], acc: Int): Int =
        if (chars.isEmpty) acc
        else if (acc < 0) acc
        else chars.head match {
          case '(' => tailBalance(chars.tail, acc+1)
          case ')' => tailBalance(chars.tail, acc-1)
          case _ => tailBalance(chars.tail, acc)
        }
      tailBalance(chars, 0) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = money match {
      case 0 => 1
      case x if x < 0 => 0
      case x if x > 0 =>
        if (coins.isEmpty) 0
        else
          countChange(money - coins.head, coins) +
            countChange(money, coins.tail)
    }
  }
