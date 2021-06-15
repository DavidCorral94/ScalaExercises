import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import shapeless._
import poly.~>

object PolymorphicFunctionValues {
  object choose extends (Seq ~> Option) {
    def apply[T](s: Seq[T]) = s.headOption
  }

  choose(Seq(1, 2, 3)) should be(Some(1))
  choose(Seq('a', 'b', 'c')) should be(Some('a'))

  def pairApply(f: Seq ~> Option) =
    (f(Seq(1, 2, 3)), f(Seq('a', 'b', 'c')))
  pairApply(choose) should be((Some(1), Some('a')))

  (List(Seq(1, 3, 5), Seq(2, 4, 6)) map choose) should be(
    List(Some(1), Some(2))
  )

}
