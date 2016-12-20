/*
Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
The first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the
previous two—the sequence begins 0, 1, 1, 2, 3, 5. Your definition should use a
local tail-recursive function.
def fib(n: Int): Int
*/

object Ex2_1 {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, next: Int): Int = {
      if (n == 1) prev  
      else go(n - 1, next, prev + next)
    }
    go(n, 0, 1)
  }
}

