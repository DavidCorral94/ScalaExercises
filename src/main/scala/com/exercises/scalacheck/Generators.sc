import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

object Generators {

  val myGen = for {
    n <- Gen.choose(10, 20)
    m <- Gen.choose(2 * n, 500)
  } yield (n, m)

  val checker = forAll(myGen) {
    case (n, m) => m >= 2 * n
  }

  checker.check()

  val vowel = Gen.oneOf('A', 'E', 'I', 'O', 'U')
  val validChars: Seq[Char] = 'A' to 'Z'

  val checkerVowels = forAll(vowel)(v => validChars.contains(v))
  checkerVowels.check()

  val smallEvenInteger =
    Gen.choose(0, 200) suchThat (_ % 2 == 0) suchThat (_ > 100)

  val checkSmallEvenIntegers =
    forAll(smallEvenInteger)(n => n % 2 == 0 && n > 100)
  checkSmallEvenIntegers.check()

  case class Foo(intValue: Int, charValue: Char)

  val fooGen = for {
    intValue <- Gen.posNum[Int]
    charValue <- Gen.alphaChar
  } yield Foo(intValue, charValue)

  val checkFoo =
    forAll(fooGen)(foo => foo.intValue > 0 && foo.charValue.isValidChar == true)
  checkFoo.check()

  val customGen = Gen.sized { size =>
    val positiveNumbers = size / 3
    val negativeNumbers = size * 2 / 3
    for {
      posNumList <- Gen.listOfN(positiveNumbers, Gen.posNum[Int])
      negNumList <- Gen.listOfN(negativeNumbers, Gen.posNum[Int].map(n => -n))
    } yield (size, posNumList, negNumList)
  }

  val checkLists = forAll(customGen) {
    case (size, positives, negatives) => {
      positives.length == size / 3 && negatives.length == size * 2 / 3
    }
  }
  checkLists.check()

  val genIntList =
    Gen.containerOf[List, Int](Gen.choose(0, 100) suchThat (_ < 50))
  val checkIntList =
    forAll(genIntList)(n =>
      if (n.nonEmpty) {
        println(n)
        n.forall(_ < 50)
      } else {
        println("stop" + n)
        n.isEmpty
      }
    )
  checkIntList.check()

}
