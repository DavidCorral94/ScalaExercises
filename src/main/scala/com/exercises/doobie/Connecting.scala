package com.exercises.doobie

import doobie._
import doobie.implicits._
import doobie.hikari._

import cats._
import cats.effect._
import cats.implicits._

object Connecting extends IOApp {

  val transactor: Resource[IO, HikariTransactor[IO]] =
    for {
      ce <- ExecutionContexts.fixedThreadPool[IO](32) // our connect EC
      be <- Blocker[IO] // our blocking EC
      xa <- HikariTransactor.newHikariTransactor[IO](
        "org.h2.Driver", // driver classname
        "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", // connect URL
        "sa", // username
        "", // password
        ce, // await connection here
        be // execute JDBC operations here
      )
    } yield xa

  override def run(args: List[String]): IO[ExitCode] = {
    val program1 = 42.pure[ConnectionIO]

    val res1 =
      transactor.use(42.pure[ConnectionIO].transact[IO]).unsafeRunSync()
    println(res1)

    val res2 = transactor
      .use(sql"select 42".query[Int].unique.transact[IO])
      .unsafeRunSync()
    println(res2)

    val largerProgram = for {
      a <- sql"select 42".query[Int].unique
      b <- sql"select power(5, 2)".query[Int].unique
    } yield (a, b)
    val res3 = transactor.use(largerProgram.transact[IO]).unsafeRunSync();
    println(res3)

    //Alternative
    val oneProgram = sql"select 42".query[Int].unique
    val anotherProgram = sql"select power(5, 2)".query[Int].unique

    val res4 = transactor.use((oneProgram, anotherProgram).mapN(_ + _).transact[IO]).unsafeRunSync()
    println(res4)

    IO.pure(ExitCode.Success)
  }
}
