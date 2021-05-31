import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

object Arbitraries {

  val evenInteger = Arbitrary.arbitrary[Int] suchThat (_ % 2 == 0)
  val checkEvenInt = forAll(evenInteger)(_ % 2 == 0)
  checkEvenInt.check()

  implicit lazy val myCharArbitrary = Arbitrary(
    Gen.oneOf('A', 'E', 'I', 'O', 'U')
  )

  // ?????? why fails
  val validChars: Seq[Char] = 'A' to 'Z'
  //val checkChars = forAll(myCharArbitrary)(validChars.contains(_))
  //val checkChars = forAll(myCharArbitrary){c: Char => validChars.contains(c)}
  //checkChars.check()

  case class Foo(intValue: Int, charValue: Char)
  val fooGen = for {
    intValue <- Gen.posNum[Int]
    charValue <- Gen.alphaChar
  } yield Foo(intValue, charValue)

  implicit lazy val fooArbitrary = Arbitrary(fooGen)

  // as before, I can not cast from Arbitrary to Foo

  val checkFoo = forAll(fooArbitrary){
    foo : Foo =>
  }

}
