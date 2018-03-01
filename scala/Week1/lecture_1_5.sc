/**
  * Square roots with Newton's method
  */
object Lecture_1_5 {

  def abs(x: Double) = if (x<0) -x else x

  def sqrt(x: Double) = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improveGuess(guess))

    def isGoodEnough(guess:Double): Boolean =
      abs(guess * guess - x) / x < 0.001

    def improveGuess(guess:Double): Double =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  sqrt(2.0)
  sqrt(9)
  sqrt(1e60)
  sqrt(1e-60)
}


