
import cats._
import cats.data.{Chain, NonEmptyChain, NonEmptyList}
import cats.implicits._

object Cats{

  // Chain

  val c1 = Chain(1,2,3)
  val c2 = c1.append(4)
  val c3 = c2.append(5)
  val c4 = c3.append(6)

  val nec = NonEmptyChain(1)

  val nel = NonEmptyList(1, List())
  nel.append(2)
}