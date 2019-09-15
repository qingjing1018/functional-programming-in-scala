package recfun

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
  def pascal(c: Int, r: Int): Int =
    if (c > r) {
      throw new Exception("Column input can't be larger than row input!")
    }
    else {
      if (c == r || c == 0) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def isBalanced(chars: List[Char], numberOfOpen: Int): Boolean =
      if (chars.isEmpty) numberOfOpen == 0
      else if (chars.head == '(')
        isBalanced(chars.tail, numberOfOpen + 1)
      else if (chars.head == ')')
        numberOfOpen > 0 && isBalanced(chars.tail, numberOfOpen - 1)
      else isBalanced(chars.tail, numberOfOpen)
    isBalanced(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
}
