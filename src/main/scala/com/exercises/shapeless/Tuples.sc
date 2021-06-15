import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import shapeless._
import shapeless.syntax.std.tuple._
import syntax.std.tuple._
import poly._

object Tuples {
  (23, "foo", true).head should be(23)
  (23, "foo", true).tail should be(("foo", true))

  /*
  // I think there is a problem with these three operations:
  // Check these tests: https://github.com/scala-exercises/exercises-shapeless/blob/main/src/test/scala/shapeless/TuplesHListExercisesSpec.scala
  (23, "foo", true).take(2) should be((23, "foo"))
  (23, "foo", true).drop(2) should be(true)
  (23, "foo", true).split(1) should be((23, ("foo", true)))*/

  val list1 = 23 +: ("foo", true)
  list1 should be((23, "foo", true))
  val list2 = (23, "foo") :+ true
  list2 should be((23, "foo", true))
  val list3 = (23, "foo") ++ (true, 2.0)
  list3 should be((23, "foo", true, 2.0))

  import poly._

  object option extends (Id ~> Option) {
    def apply[T](t: T) = Option(t)
  }
  val list4 = (23, "foo", true) map option
  list4 should be((Some(23), Some("foo"), Some(true)))

  val list5 = ((23, "foo"), (), (true, 2.0)) flatMap identity
  list5 should be((23, "foo", true, 2.0))

  val res = (23, "foo", true).productElements
  res should be(23 :: "foo" :: true :: HNil)

  (23, "foo", true).toList should be(List(23, "foo", true))

  import syntax.zipper._
  val l = (23, ("foo", true), 2.0).toZipper.right.down.put("bar").root.reify

}
