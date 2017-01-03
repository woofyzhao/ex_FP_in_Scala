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
      if (c == 0 || c == r) 1
      else pascal(c, r-1) + pascal(c-1, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      
      @annotation.tailrec
      def loop(chars: List[Char], cnt: Int) : Boolean = {
        if (chars.isEmpty) {
          if (cnt == 0) true else false
        }
        else if (cnt < 0) false
        else {
          if (chars(0) == '(') loop(chars.tail, cnt + 1)
          else if (chars(0) == ')') loop(chars.tail, cnt - 1)
          else loop(chars.tail, cnt)
        } 
      }
      
      loop(chars, 0)
    }
  
  /**
   * Exercise 3
   */


    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1 
      else if (coins.isEmpty) 0
      else {
        var sum = 0
        val t = (money / coins(0)).toInt
        for (i <- 0 to t) {
          sum += countChange(money - i * coins(0), coins.tail)
        }
        sum
      }
    }

   
    /*
    def countChange(money: Int, coins: List[Int]): Int = {
      
      @annotation.tailrec
      def sum(acc: Int, f: (Int, List[Int]) => Int, money:Int, coins: List[Int]): Int = {
        if (money <= 0) acc
        else sum(acc + f(money, coins.tail), f, money - coins(0), coins)
      }
      
      if (money == 0) 1 
      else if (coins.isEmpty) 0
      else {
        sum(0, countChange, money, coins)
      }
    }
    */    
  }
