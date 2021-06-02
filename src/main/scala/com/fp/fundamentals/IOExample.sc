import cats.effect.IO

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object IOExample {

  case class MyIO[A](private val value: () => A) {
    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO { () =>
        val a = value()
        val iob = f(a)
        iob.value()
      }

    def map[B](f: A => B): MyIO[B] =
      MyIO { () =>
        f(value())
      }

    def unsafeRun(): A = value()
  }

  val ioHello = MyIO { () => println("Hello World!") }
  val futureHello = Future { println("Hello World!") }

  val programIO = for {
    _ <- ioHello
    _ <- ioHello
  } yield ()

  programIO.unsafeRun()

  val programFuture = for {
    _ <- futureHello
    _ <- futureHello
  } yield ()


  // Running IOs

  import cats.effect._
  import cats.syntax.all._
  import scala.concurrent.duration._

  val ioLife = IO.pure(42)
  ioLife.unsafeRunSync()

  ioLife.unsafeRunTimed(5.seconds)
  ioLife.unsafeRunAsync({
    case Left(e) => println(s"There was an exception $e")
    case Right(a) => println(s"All right! $a")
  })

  ioLife.unsafeToFuture()


  // Use IOApp is safer, because I don't need to call unsafe operators
  object MyApp extends IOApp {
    override def run(args: List[String]) : IO[ExitCode] = {
      IO.delay(println("Say no to unsafe ops")).as(ExitCode.Success)
    }
  }


  // Using Sync with IO

  def program[F[_]](implicit  F: Sync[F]): F[Unit] = {
    val f1: F[String] = F.pure("hello")

    val boom : F[Int] = Sync[F].raiseError(new RuntimeException("Booom"))
    val safe: F[Either[Throwable, Int]] = boom.attempt

    val f3: F[Unit] = F.delay(println("Last task!"))

    f1 *> safe *> f3
  }

  program[IO].unsafeRunSync()

  //ContextShift
  // If uncomment, infinite loop
  def doStuff[F[_]: Sync](s: String): F[Unit] =
    Sync[F].delay(println(s))

  def repeat[F[_]: Sync](s: String): F[Unit] =
    for {
      _ <- doStuff[F](s)
      //_ <- repeat[F](s)
    } yield ()

  implicit val contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.fromExecutor(
      java.util.concurrent.Executors.newSingleThreadExecutor()
    ))

  repeat[IO]("A").unsafeToFuture()
  repeat[IO]("B").unsafeToFuture()

  // Alternative using ContextShift
  def fairRepeat[F[_]: Sync: ContextShift](s: String): F[Unit] =
    for {
      _ <- doStuff(s)
      _ <- ContextShift[F].shift
      _ <- fairRepeat[F](s)
    } yield ()

  fairRepeat[IO]("A").unsafeToFuture()
  fairRepeat[IO]("B").unsafeToFuture()

  // Blocker
  //implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val blockerRes: Resource[IO, Blocker] = Blocker[IO]
  blockerRes.use { blocker =>
    blocker.blockOn(IO(println("Hello")))
  }


}
