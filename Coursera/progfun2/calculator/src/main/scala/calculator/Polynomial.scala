package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      b() * b() - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      if (delta() < 0) Set() 
      else if (delta() == 0) Set(-b() / a() / 2)
      else Set((-b() + Math.sqrt(delta())) / a() / 2, (-b() - Math.sqrt(delta())) / a() / 2)
    }
  }
}
