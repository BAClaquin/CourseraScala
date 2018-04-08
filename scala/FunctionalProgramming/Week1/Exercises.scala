package FunctionalProgramming.Week1

import scala.annotation.tailrec

/**
  *  Recursion exercises
  */
object Exercises {

  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }


    println()
    println("Balance")
    // true
    println(balance("(if (zero? x) max (/ 1 x))".toList))
    // true
    println(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
    // false
    println(balance(":-)".toList))
    // false
    println(balance("())(".toList))


    println()
    println("countChange")
    println(countChange(4,List(1,2)) == 3)
    println(countChange(300,List(5,10,20,50,100,200,500)) == 1022)
    println(countChange(301,List(5,10,20,50,100,200,500)) == 0)
    println(countChange(300,List(500,5,50,100,20,200,10)) == 1022)
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || r == c) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    // tail recursive function
    @tailrec
    def loop(chars: List[Char], state: Integer): Integer =
      if (chars.isEmpty || state < 0) state
      else if (chars.head == '(') loop(chars.tail, state + 1)
      else if (chars.head == ')') loop(chars.tail, state - 1)
      else loop(chars.tail, state)

    loop(chars, 0) == 0
  }


  /**
    * Exercise 3
    */
  def countChange(money: Integer, coins: List[Integer]): Integer = {
    if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

}