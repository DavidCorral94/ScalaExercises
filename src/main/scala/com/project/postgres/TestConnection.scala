package com.project.postgres

import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource}
import cats.implicits.catsSyntaxApplicativeId
import doobie.{ConnectionIO, ExecutionContexts}
import doobie.hikari.HikariTransactor
import doobie.implicits._

object TestConnection extends IOApp{

  val transactor: Resource[IO, HikariTransactor[IO]] =
    for {
      ce <- ExecutionContexts.fixedThreadPool[IO](32) // our connect EC
      be <- Blocker[IO] // our blocking EC
      xa <- HikariTransactor.newHikariTransactor[IO](
        "org.postgresql.Driver", // driver classname
        "jdbc:postgresql:test", // connect URL
        "postgres", // username
        "postgres", // password
        ce, // await connection here
        be // execute JDBC operations here
      )
    } yield xa

  override def run(args: List[String]): IO[ExitCode] = {
    val program1 = 42.pure[ConnectionIO]

    val res1 =
      transactor.use(42.pure[ConnectionIO].transact[IO]).unsafeRunSync()
    println(res1)
    IO.pure(ExitCode.Success)
  }
}
