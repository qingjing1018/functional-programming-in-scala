import math.abs

object exercise {
  val tolerance = 0.0001
  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) / x < tolerance

  def fixPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      //  println("guess =" + guess )
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }
  def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2
  def sqrt(x: Double): Double = {
    fixPoint(averageDamp(y => x / y))(1)
  }
  //sqrt(2)
}

//exercise.fixPoint(x => 1 + x/2)(1)

exercise.sqrt(5)



// Write a square root function using fixed point and average damp

