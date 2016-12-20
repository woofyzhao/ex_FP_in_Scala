/*
Implement the higher-order function that composes two functions.
def compose[A,B,C](f: B => C, g: A => B): A => C
*/

object Ex2_5 {
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
