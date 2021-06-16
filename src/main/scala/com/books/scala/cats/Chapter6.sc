import cats.implicits.{catsSyntaxTuple2Semigroupal, catsSyntaxTuple3Semigroupal}

object Chapter6 {

  // Semigroup allows us to join values while semigroupal allow us to join contexts

  import cats.Semigroupal
  import cats.instances.option._ // for Semigroupal

  Semigroupal[Option].product(Some(123), Some("abc"))
  Semigroupal[Option].product(Some(123), None)

  Semigroupal.map3(List(1), List(2), List(3))(
    _.toString + _.toString + _.toString
  )

  val a = Semigroupal.tuple3(List(1), List(2), List(3))

  val b = (List(1), List(2), List(3)).tupled

  a == b

  final case class Cat(name: String, born: Int, color: String)

  (
    Option("Garfield"),
    Option(1978),
    Option("Orange & black")
  ).mapN(Cat.apply)

  def sum(a: Int, b: Int) : Int = a + b

  (Option(1), Option(2)).mapN(sum)

}

object Chapter6_4{
  import cats.syntax.apply._ // for tupled
  import cats.instances.vector._ // for Semigroup on Vector

  type ErrorOr[A] = Either[Vector[String], A]

  val error1: ErrorOr[Int] = Left(Vector("Error 1"))
  val error2: ErrorOr[Int] = Left(Vector("Error 2"))

  // This is fail fast, it 'crashes' at the first element
  (error1, error2).tupled

  // In order to solve it, we can create the tuple in parallel
  import cats.syntax.parallel._
  (error1, error2).parTupled

  type ErrorOrList[A] = Either[List[String], A]
  val errStr1: ErrorOrList[Int] = Left(List("error 1"))
  val errStr2: ErrorOrList[Int] = Left(List("error 2"))

  (errStr1, errStr2).parTupled

  // mapMapN to map values in paralallel
  val success1: ErrorOr[Int] = Right(1)
  val success2: ErrorOr[Int] = Right(2)
  val addTwo = (x: Int, y: Int) => x + y

  (error1, error2).parMapN(addTwo)
  (success1, success2).parMapN(addTwo)

  import cats.instances.list._

  // IMPORTANT DIFFERENCES
  (List(1, 2), List(3, 4)).tupled
  (List(1, 2), List(3, 4)).parTupled
}
