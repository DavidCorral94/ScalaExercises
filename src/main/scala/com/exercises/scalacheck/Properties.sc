import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop._
import org.scalacheck.Test._
import org.scalacheck.Gen._
import org.scalacheck.Prop.exception.==>
import org.scalacheck.Prop.propBoolean

object Properties {
  val propConcatLists = forAll { (l1: List[Int], l2: List[Int]) =>
    l1.size + l2.size == (l1 ::: l2).size
  }
  propConcatLists.check()

  val propSqrt = forAll { (n: Int) => scala.math.sqrt(n * n) == n }
  propSqrt.check()

  val concatStrings =
    forAll((s1: String, s2: String) => (s1 + s2).endsWith(s2))
  concatStrings.check()

  val smallIntegers = Gen.choose(0, 100)
  val checkIntegers = forAll(smallIntegers)(n => n >= 0 && n <= 100)

}
