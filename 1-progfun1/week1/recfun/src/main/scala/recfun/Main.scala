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
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    require(c >= 0 && c <= r + 1)
    c match {
      case 0 => 1
      case `r` => 1
      case n => pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def inner(remainingChars: List[Char], open: Int): Boolean = {
      remainingChars match {
        case Nil => open == 0
        case '(' :: t => inner(t, open + 1)
        case ')' :: t => if (open == 0) false else inner(t, open - 1)
        case _ :: t => inner(t, open)
      }
    }
    inner(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def inner(remaining: Int, coinsLeft: List[Int]): Int = {
      coinsLeft.map{coin =>
        remaining - coin match {
          case n if n < 0 => 0
          case 0 => 1
          case n => inner(n, coinsLeft.filter(_ <= coin))
        }
      }.sum
    }
    money match {
      case n if n < 0 => 0
      case 0 => 1
      case n => inner(n, coins)
    }
  }
}
