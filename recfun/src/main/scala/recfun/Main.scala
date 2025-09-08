package recfun
import common._

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
  // 파스칼 삼각형의 (c, r) 위치의 값 계산
  def pascal(c: Int, r: Int): Int = {
    require(c >= 0, "col num should be non-negative integer")
    require(r >= 0, "row num should be non-negative integer")
    require(c <= r, "col num cannot exceed row num")

    if(c == 0 || c == r) 1 // top or edge of the triangle
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
   * Exercise 2
   */
  // 주어진 문자열의 () 괄호가 올바르게 균형을 이루는지 확인
  def balance(chars: List[Char]): Boolean = {
    def balanceChecker(chars: List[Char], balance: Int): Boolean = {
      if (balance < 0) false // closing parenthesis ')' appears first
      else if (chars.isEmpty) balance == 0 // true if fully balanced
      else chars.head match {
        case '(' => balanceChecker(chars.tail, balance + 1)
        case ')' => balanceChecker(chars.tail, balance - 1)
        case _ => balanceChecker(chars.tail, balance)
      }
    }

    balanceChecker(chars, 0)
  }

  /**
   * Exercise 3
   */
  // 주어진 금액을, 주어진 동전 단위들로 거슬러 줄 수 있는 모든 경우의 수 계산
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0 ) 1 // valid combination
    else if (money < 0 || coins.isEmpty) 0 // invalid combination
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
