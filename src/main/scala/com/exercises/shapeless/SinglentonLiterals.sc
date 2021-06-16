object SinglentonLiterals {
  import shapeless._
  import syntax.std.tuple._

  val hlist = 23 :: "foo" :: true :: HNil
  //hlist(1) == "foo"

  val tuple = (23, "foo", true)
  //tuple(1) == "foo"

  val (wTrue, wFalse) = (Witness(true), Witness(false))

  type True = wTrue.T
  type False = wFalse.T

  trait Select[B] { type Out }

  implicit val selInt = new Select[True] { type Out = Int }
  implicit val selString = new Select[False] { type Out = String }

  def select(b: WitnessWith[Select])(t: b.instance.Out) = t

}
