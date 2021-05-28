object Applys {

  import cats._

  val intToString: Int => String = _.toString
  val double: Int => Int = _ * 2
  val addTwo: Int => Int = _ + 2

  Apply[Option].map(Some(1))(intToString) == Some("1")
  Apply[Option].map(Some(1))(double) == Some(2)
  Apply[Option].map(Some(1))(addTwo) == Some(3)

  val op = Some(1)

  // Whats the point? We can map already over Option, no need of Apply
  Apply[Option].map(Some(1))(intToString) == op.map(intToString)

  val listOpt = Apply[List] compose Apply[Option]
  val plusOne = (x: Int) => x + 1
  listOpt.ap(List(Some(plusOne)))(List(Some(1), None, Some(3))) == List(
    Some(2),
    None,
    Some(4)
  )

  // Alternative legit (much more complicated)
  List(Some(1), None, Some(3)).map(x => {
    val n = x
    if (n.isEmpty)
      None
    else
      Some(n.get + 1)
  }) == listOpt.ap(List(Some(plusOne)))(List(Some(1), None, Some(3)))

  val addArity2 = (a: Int, b: Int) => a + b
  Apply[Option].ap2(Some(addArity2))(Some(1), Some(2)) == Some(3)

  val addArity3 = (a: Int, b: Int, c: Int) => a + b + c
  Apply[Option].ap3(Some(addArity3))(Some(1), Some(2), Some(3))

  val concatSomes = (a: String, b: String, c: String, d: String) =>
    a.concat(b).concat(c).concat(d)

  Apply[Option]
    .ap4(Some(concatSomes))(Some("a"), Some("b"), Some("c"), Some("d"))

  Apply[Option].map2(Some(1), Some(2))(addArity2) ==
    Apply[Option].ap2(Some(addArity2))(Some(1), Some(2))

  Apply[Option].tuple2(Some(1), Some(2))

  Apply[Option].tuple2(Some("Hola"), Some("k tal"))

  val option2 = (Option(1), Option(2))
  val option3 = (option2._1, option2._2, Option.empty[Int])

  import cats.implicits._

  (option2 mapN addArity2) == Option(3)
  (option3 mapN addArity3) == None

  (option2 apWith Some(addArity2)) == Option(3)
  (option3 apWith Some(addArity3)) == None

  option2.tupled == Some(1,2)
  option3.tupled == None

}
