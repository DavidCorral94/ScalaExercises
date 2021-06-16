object Arity {
  import shapeless._
  import syntax.std.function._
  import ops.function._

  def applyProduct[P <: Product, F, L <: HList, R](
      p: P
  )(f: F)(implicit gen: Generic.Aux[P, L], fp: FnToProduct.Aux[F, L => R]) =
    f.toProduct(gen.to(p))

  applyProduct((1, 2))((_: Int) + (_: Int)) == 3
  applyProduct((1, 2, 3))((_: Int) * (_: Int) * (_: Int)) == 6
}
