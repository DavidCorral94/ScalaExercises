import cats.implicits.{catsSyntaxApplicativeErrorId, catsSyntaxApplicativeId}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Chapter4_1 {
  // <3 MONADS <3

  // Exercise 4.1.2

  trait Monad[F[_]] {
    def pure[A](a: A): F[A]

    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

    def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(a => pure(func(a)))
  }
}

object Chapter4_2 {
  // <3 MONADS <3 in Cats
  import cats.Monad
  import cats.instances.option._ // for Monad
  import cats.instances.list._ // for Monad

  val opt1 = Monad[Option].pure(1)
  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 4))
  val opt3 = Monad[Option].map(opt1)(a => a * 4)

  val list1 = Monad[List].pure(10)
  val list2 = Monad[List].flatMap(list1)(a => List(a, a + 1))
  val list3 = Monad[List].map(list2)(a => a * 2)

  import cats.instances.option._ // for Monad
  import cats.instances.list._ // for Monad
  import cats.syntax.applicative._ // for pure

  // Implicit syntax vs Specific syntax
  1.pure[Option] == Monad[Option].pure(1)
  1.pure[List] == Monad[List].pure(1)

  import cats.Monad
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = {
    a.flatMap(x => b.map(y => x * x + y * y))
  }

  sumSquare(Option(1), Option(2))
  sumSquare(List(1, 2), List(2, 3))

  // This is the same but simplified

  def sumSquareSimplier[F[_]: Monad](a: F[Int], b: F[Int]) =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

  sumSquareSimplier(Option(1), Option(2))
  sumSquareSimplier(List(1, 2), List(2, 3))

}

object Chapter4_3 {
  import cats.Monad
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]) =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

  // We can't do because 1 and 2 (Int) are not Monads
  //Chapter4_2.sumSquare(1,2)

  // But Cats provides the wonderful Id
  import cats.Id
  sumSquare(1: Id[Int], 2: Id[Int])

  // And we can simplify this even more
  type Id[A] = A
  val a1: Id[Int] = 1
  val a2: Id[Int] = 2
  sumSquare(a1, a2)

  val pureInt = Monad[Id].pure(2)
  val pureList = Monad[Id].pure(List(1, 2, 3))

  for {
    x <- pureInt
    y <- pureList
  } yield y * x

  // Exercise 4.3.1
  // We can do this stuff, directly, because we previously defined
  // type Id[A] = A
  // so substitution does its work :D
  def pure[A](value: A): Id[A] =
    value

  def map[A, B](initial: Id[A])(func: A => B): Id[B] =
    func(initial)

  def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] =
    func(initial)
}

object Chapter4_5 {
  import cats.MonadError
  import cats.instances.either._

  type ErrorOr[A] = Either[String, A]

  val monadError = MonadError[ErrorOr, String]

  val success = monadError.pure(42)
  val failure = monadError.raiseError("Badness")

  monadError.handleErrorWith(failure) {
    case "Badness" => monadError.pure("It's ok...")
    case _         => monadError.pure("It's not ok!!!!!")
  }

  monadError.ensure(success)("Number too low!")(_ > 1000)

  import scala.util.Try
  import cats.instances.try_._ // for MonadError

  val exn: Throwable =
    new RuntimeException("It's all gone wrong")

  exn.raiseError[Try, Int]

  def validateAdult[F[_]](
      age: Int
  )(implicit me: MonadError[F, Throwable]): F[Int] =
    if (age >= 18)
      me.pure(age)
    else
      me.raiseError(
        new IllegalArgumentException("Age must be greater than or equal to 18")
      )

  validateAdult[Try](18)
  validateAdult[Try](8)
  type ExceptionOr[A] = Either[Throwable, A]
  validateAdult[ExceptionOr](-1)
}

object Chapter4_6 {

  import cats.Eval

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(
      fn: (A, Eval[B]) => Eval[B]
  ): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil =>
        acc
    }

  // Eval stack safe
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }.value

  foldRight((1 to 100000).toList, 0L)(_ + _)

}

object Chapter4_7 {
  import cats.data.Writer
  import cats.instances.vector._ // for Monoid

  Writer(
    Vector(
      "It was the best of times",
      "it was the worst of times"
    ),
    1859
  )

  import cats.instances.vector._ // for Monoid
  import cats.syntax.applicative._ // for pure

  type Logged[A] = Writer[Vector[String], A]

  123.pure[Logged]

  import cats.syntax.writer._ // for tell

  Vector("msg1", "msg2", "msg3").tell

  import cats.syntax.writer._ // for writer

  val a = Writer(Vector("msg1", "msg2", "msg3"), 123)

  val b = 123.writer(Vector("msg1", "msg2", "msg3"))

  val aResult: Int =
    a.value

  val aLog = a.written

  val (log, result) = b.run

  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b

  writer1.run

  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))

  val writer3 = writer1.bimap(
    log => log.map(_.toUpperCase),
    res => res * 100
  )

  writer3.run

  val writer4 = writer1.mapBoth { (log, res) =>
    val log2 = log.map(_ + "!")
    val res2 = res * 1000
    (log2, res2)
  }

  writer4.run

  val writer5 = writer1.reset

  writer5.run

  val writer6 = writer1.swap

  writer6.run

  // Ej 4.7.3

  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(1000)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  //factorial(5)

  type Logger[A] = Writer[Vector[String], A]
  def factorialLogged(n: Int): Logged[Int] =
    for {
      ans <-
        if (n == 0)
          10.pure[Logged]
        else
          slowly(factorialLogged(n - 1).map(_ * n))
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  factorialLogged(5).run

  import scala.concurrent.ExecutionContext.Implicits.global

  Await.result(
    Future
      .sequence(
        Vector(
          Future(factorialLogged(5)),
          Future(factorialLogged(5))
        )
      )
      .map(_.map(_.written)),
    5.seconds
  )
}

object Chapter4_8 {
  import cats.data.Reader

  final case class Cat(name: String, favoriteFood: String)

  val catName: Reader[Cat, String] =
    Reader(cat => cat.name)

  catName.run(Cat("Garfield", "lasagne"))

  val greetKitty: Reader[Cat, String] =
    catName.map(name => s"Hello ${name}")

  greetKitty.run(Cat("Heathcliff", "junk food"))

  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed <- feedKitty
    } yield s"$greet. $feed."

  greetAndFeed(Cat("Garfield", "lasagne"))

  greetAndFeed(Cat("Heathcliff", "junk food"))

  // Ej 4.8.3
  final case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db =>
      db.passwords.get(username) match {
        case Some(p) => p == password
        case None    => false
      }
    )

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      maybeUsername <- findUsername(userId)
      resLogin <-
        maybeUsername
          .map { u =>
            checkPassword(u, password)
          }
          .getOrElse {
            false.pure[DbReader]
          }
    } yield resLogin

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  checkLogin(1, "zerocool").run(db)
  checkLogin(4, "davinci").run(db)
}

object Chapter4_9 {
  import cats.data.State

  val a = State[Int, String] { state =>
    (state, s"The state is $state")
  }

  // Get the state and the result:
  val (state, result) = a.run(10).value

  // Get the state, ignore the result:
  val justTheState = a.runS(10).value

  // Get the result, ignore the state:
  val justTheResult = a.runA(10).value

  // Composing and transforming state
  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }

  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)

  val (state2, result2) = both.run(20).value

  /**
    * get - extracts the state as the result;
    * set - updates the state and returns unit as the result;
    * pure - ignores the state and returns a supplied result;
    * inspect - extracts the state via a transformation function;
    * modify - updates the state using an update function.
    */

  val getDemo = State.get[Int]
  getDemo.run(10).value

  val setDemo = State.set[Int](30)
  setDemo.run(10).value

  val pureDemo = State.pure[Int, String]("Result")
  pureDemo.run(10).value

  val inspectDemo = State.inspect[Int, String](x => s"${x}!")
  inspectDemo.run(10).value

  val modifyDemo = State.modify[Int](_ + 1)
  modifyDemo.run(10).value

  // Equivalent
  import cats.data.State._

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)

  val (state3, result3) = program.run(1).value

  // Ej 4.9.3
  import cats.data.State

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ + _)
      case "*" => operator(_ + _)
      case "/" => operator(_ + _)
      case num => operand(num.toInt)
    }

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  def operator(function: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: tail =>
        val ans = function(a, b)
        (ans :: tail, ans)
      case _ => sys.error("Fail!")
    }

  val programEval = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans

  programEval.runA(Nil).value

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState])((a, b) => a.flatMap(_ => evalOne(b)))

  val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))

  multistageProgram.runA(Nil).value

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  evalInput("1 2 + 3 4 + *")
}

object Chapter4_10 {
  import cats.Monad

  // Ej 4.10.1
  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  implicit val monadForTree = new Monad[Tree] {
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]) =
      fa match {
        case Leaf(value)         => f(value)
        case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]) =
      flatMap(f(a)) {
        case Left(value)  => tailRecM(value)(f)
        case Right(value) => Leaf(value)
      }

    override def pure[A](x: A) =
      Leaf(x)

  }

  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

  branch(leaf(100), leaf(200)).flatMap(x => branch(leaf(x - 1), leaf(x + 1)))

  for {
    a <- branch(leaf(100), leaf(200))
    b <- branch(leaf(a - 10), leaf(a + 10))
    c <- branch(leaf(b - 1), leaf(b + 1))
  } yield c
}
