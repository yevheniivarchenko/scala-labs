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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def function(chars: List[Char], openParentheses: Int): Int = {
      if (chars.isEmpty || openParentheses < 0)
        openParentheses
      else if (chars.head == '(')
        function(chars.tail, openParentheses + 1)
      else if (chars.head == ')')
        function(chars.tail, openParentheses - 1)
      else
        function(chars.tail, openParentheses)
    }

    function(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty)
      0
    else if (money == 0)
      1
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

  /**
   * Exercise 4
   */
  def ci(a: Int, x: Int, k: Int): Double = {
    if (x > a)
      a
    else if (x < a && k >= 1)
      a * scala.math.pow(x, k) + ci(a, x, k - 1)
    else
      0
  }
}
