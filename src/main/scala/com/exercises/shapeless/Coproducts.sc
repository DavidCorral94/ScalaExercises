import shapeless.Poly1

object sizeM extends Poly1 {
  implicit def caseInt = at[Int](i => (i, i))
  implicit def caseString = at[String](s => (s, s.length))
  implicit def caseBoolean = at[Boolean](b => (b, 1))
}

object Coproducts {
  import shapeless._

  type ISB = Int :+: String :+: Boolean :+: CNil

  val isb = Coproduct[ISB]("foo")

  isb.select[Int] == None
  isb.select[String] == Some("foo")

  val m = isb map sizeM

  m.select[(String, Int)] == ("foo", 3)
  

}
