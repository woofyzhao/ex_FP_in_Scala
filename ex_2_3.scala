/*
Let’s look at another example, currying,9 which converts a function f of two arguments
into a function of one argument that partially applies f. Here again there’s only one
implementation that compiles. Write this implementation.
def curry[A,B,C](f: (A, B) => C): A => (B => C)
*/

object Ex2_3 {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }
}
