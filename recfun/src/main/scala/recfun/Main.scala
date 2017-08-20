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
    var a = 1;
    if (r == 0 && c == 0) a = 1
    else if (c > r || r < 0) a = 0
    else {
      a = (pascal(c - 1, r - 1) + pascal(c, r - 1))
    }
    a
  }


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def iter(chars: List[Char], acc: Int): Boolean = {
      if(acc<0) false
      else if (chars.isEmpty)
        if (acc != 0) false else true
      else if (chars.head == '(') iter(chars.tail, acc + 1)
      else if (chars.head == ')') iter(chars.tail, acc - 1)
      else
        iter(chars.tail, acc)
    }

    iter(chars, 0);
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}