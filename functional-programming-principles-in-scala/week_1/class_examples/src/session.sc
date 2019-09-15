object session {
  def abs(x: Double): Double =
    if (x < 0) -x else x
  def sqrt(x: Double): Double = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double): Double =
      (guess + x / guess) / 2

    sqrtIter(guess = 1.0)
  }
}

session.sqrt(1e-6)

session.sqrt(1e20)