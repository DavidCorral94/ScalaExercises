import scala.math.abs

object ScalaTutorial {
  16.toHexString

  def loop: Int = loop

  def x = loop


  def sqrtv2(guess: Double, n: Double): Double = {
    if (isGood(guess, n))
      guess
    else
      sqrtv2(improve(guess, n), n)
  }

  def isGood(guess: Double, n: Double) =
    abs(guess * guess - n) < 0.00000001

  def improve(guess: Double, n: Double) =
    (guess + n / guess) / 2

  def sqrt(x: Double) = sqrtv2(1.0, x)

  sqrt(4.0)

}