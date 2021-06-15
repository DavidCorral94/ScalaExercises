package com.exercises.doobie

import cats.effect.ExitCode.Success
import cats.effect.{ExitCode, IO, IOApp}
import doobie.ConnectionIO
import doobie.implicits.toSqlInterpolator
import doobie.implicits._
import com.exercises.doobie.Connecting.transactor
import cats.implicits._
import com.exercises.doobie.DoobieUtils.PeopleTable._
import com.exercises.doobie.Model.People
import doobie.postgres.sqlstate.class23.{FOREIGN_KEY_VIOLATION, UNIQUE_VIOLATION}

object ErrorHandling extends IOApp {

  def transactorBlock[A](f: => ConnectionIO[A]): IO[A] =
    transactor.use((dropPeopleTable *> createPeopleTable *> f).transact[IO])

  def findPersonById(id: Long): ConnectionIO[People] =
    sql"select id, name, age from people where id=$id"
      .query[People]
      .unique

  def insert(n: String, a: Option[Int]): ConnectionIO[Long] =
    sql"insert into people (name, age) values ($n, $a)".update
      .withUniqueGeneratedKeys("id")

  def safeInsert(
      name: String,
      age: Option[Int]
  ): ConnectionIO[Either[String, Long]] =
    insert(name, age).attemptSql.map {
      case Left(_)      => Left("Oops!")
      case Right(value) => Right(value)
    }

  def safeInsert2(
      name: String,
      age: Option[Int]
  ): ConnectionIO[Either[String, Long]] =
    insert(name, age)
      .attemptSomeSqlState {
        case FOREIGN_KEY_VIOLATION => "Another error"
        case UNIQUE_VIOLATION      => "John is already here!"
      }

  def safeInsert3(name: String, age: Option[Int]): ConnectionIO[Long] =
    insert(name, age)
      .exceptSqlState {
        case UNIQUE_VIOLATION => insert(name + "_20", age)
      }

  override def run(args: List[String]): IO[ExitCode] = {

    val insertedRows = for {
      john <- safeInsert("John", Option(35))
      otherJohn <- safeInsert("John", Option(20))
    } yield otherJohn

    val result = transactorBlock(insertedRows).unsafeRunSync()
    assert(result.isLeft)

    val insertedRows2 = for {
      john <- safeInsert2("John", Option(35))
      otherJohn <- safeInsert2("John", Option(20))
    } yield otherJohn

    val result2 = transactorBlock(insertedRows2).unsafeRunSync()
    assert(result2 == Left("John is already here!"))

    val insertedRows3 = for {
      john <- safeInsert3("John", Option(35))
      otherJohn <- safeInsert3("John", Option(20))
      info <- findPersonById(otherJohn)
    } yield info

    val result3 = transactorBlock(insertedRows3).unsafeRunSync()
    assert(result3 == People(3, "John_20", 20))
    IO.pure(Success)
  }
}
