import cats.~>
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.matchers.should.Matchers.not.be
import shapeless._
import poly._

object HeterogeneousLists {

  val sets = Set(1) :: Set("foo") :: HNil

  //val opts = sets.map(choose)

  import poly.identity

  val l: (Int :: String :: HNil) :: HNil.type :: (Boolean :: HNil) :: HNil =
    (23 :: "foo" :: HNil) :: HNil :: (true :: HNil) :: HNil

  (l flatMap identity) == (23 :: "foo" :: true :: HNil)

  import syntax.zipper._

  val l2 = 1 :: "foo" :: 3.0 :: HNil
  l2.toZipper.right.put(("wibble", 45)).reify
  l2.toZipper.right.delete.reify

  trait Fruit
  case class Apple() extends Fruit
  case class Pear() extends Fruit

  type FFFF = Fruit :: Fruit :: Fruit :: Fruit :: HNil
  type APAP = Apple :: Pear :: Apple :: Pear :: HNil

  val a: Apple = Apple()
  val p: Pear = Pear()

  val apap: APAP = a :: p :: a :: p :: HNil

  import scala.reflect.runtime.universe._

  implicitly[TypeTag[APAP]].tpe.typeConstructor <:< typeOf[FFFF] == true

  import syntax.typeable._

  val ffff: FFFF = apap.unify
  val precise: Option[APAP] = ffff.cast[APAP]

}
