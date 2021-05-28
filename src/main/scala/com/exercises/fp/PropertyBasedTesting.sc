import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

object PropertyBasedTesting {
  val genString = Gen.hexStr

  val propA = forAll(genString)(s => s.substring(0) == s.charAt(0).toString)
  val propB = forAll(genString)(s => s.startsWith(s) == true)

  val res = (propA && propB)

  val s = genString.toString

  s.substring(s.length - 1)
  s.charAt(s.length -1) .toString





}
