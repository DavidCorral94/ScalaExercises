package com.exercises.doobie

import cats.effect.ExitCode.Success
import cats.effect.{ExitCode, IO, IOApp}
import doobie.{ConnectionIO, Update, Update0}
import doobie.implicits.toSqlInterpolator
import doobie.implicits._
import com.exercises.doobie.Connecting.transactor
import com.exercises.doobie.Model._
import cats.implicits._
import com.exercises.doobie.DoobieUtils.PeopleTable._

object InsertingAndUpdating extends IOApp {

  def transactorBlock[A](f: => ConnectionIO[A]): IO[A] =
    transactor.use((dropPeopleTable *> createPeopleTable *> f).transact[IO])

  val drop: Update0 =
    sql"""DROP TABLE IF EXISTS people""".update

  val create: Update0 =
    sql"""CREATE TABLE people (id SERIAL, name VARCHAR NOT NULL UNIQUE, age SMALLINT)""".update

  // Drop and create table
  // (drop, create).mapN(_ + _).transact(xa).unsafeRunSync

  // Method to insert data
  def insert1(id: Int, name: String, age: Option[Int]): Update0 =
    sql"insert into people (id, name, age) values ($id,$name, $age)".update

  // Method to insert and get the inserted row
  def insert2(name: String, age: Option[Int]): ConnectionIO[People] =
    for {
      _ <- sql"insert into people (name, age) values ($name, $age)".update.run
      id <- sql"select lastval()".query[Long].unique
      p <-
        sql"select id, name, age from people where id = $id"
          .query[People]
          .unique
    } yield p

  // Method to insert and get the inserted row (better way)
  def insert2_H2(
      id: Int,
      name: String,
      age: Option[Int]
  ): ConnectionIO[People] =
    for {
      id <-
        sql"insert into people (id,name, age) values ($id,$name, $age)".update
          .withUniqueGeneratedKeys[Int]("id")
      p <-
        sql"select id, name, age from people where id = $id"
          .query[People]
          .unique
    } yield p

  // Method to insert and get the inserted row (PostgreSQL way)
  def insert3(id: Int, name: String, age: Option[Int]): ConnectionIO[People] = {
    sql"insert into people (id,name, age) values ($id,$name, $age)".update
      .withUniqueGeneratedKeys("id", "name", "age")
  }

  // Same for updates
  val up = {
    sql"update people set age = age + 1 where age is not null".update
      .withGeneratedKeys[People]("id", "name", "age")
  }

  // Batch updates
  type PeopleInfo = (String, Option[Short])

  def insertMany(ps: List[PeopleInfo]): ConnectionIO[Int] = {
    val sql = "insert into people (name, age) values (?, ?)"
    Update[PeopleInfo](sql).updateMany(ps)
  }

  override def run(args: List[String]): IO[ExitCode] = {

    // Inserting data
    val insertedOnePeople = insert1(1, "Alice", Option(12)).run

    val insertedOtherPeople = insert1(2, "Bob", None).run

    val insertedRows =
      transactorBlock((insertedOnePeople, insertedOtherPeople).mapN(_ + _))
        .unsafeRunSync()

    assert(insertedRows == 2)

    val rows = for {
      row1 <- insert1(1, "Alice", Option(12)).run
      row2 <- insert1(2, "Bob", None).run
      row3 <- insert1(3, "John", Option(17)).run
    } yield row1 + row2 + row3

    val insertedRowsFor = transactorBlock(rows).unsafeRunSync()

    assert(insertedRowsFor == 3)

    val data =
      List(
        (1, "Alice", Option(12)),
        (2, "Bob", None),
        (3, "John", Option(17)),
        (4, "Mary", Option(16))
      )

    val insertedRowsList =
      transactorBlock(data.traverse(item => (insert1 _).tupled(item).run))
        .unsafeRunSync()

    assert(insertedRowsList.length == 4)

    // Updating data
    val result = for {
      insertedRowsUp <- insert1(1, "Alice", Option(12)).run
      updatedRows <-
        sql"update people set age = 15 where name = 'Alice'".update.run
      people <-
        sql"select id, name, age from people where name = 'Alice'"
          .query[People]
          .unique
    } yield (insertedRowsUp, updatedRows, people)

    val (insertedRowsUp, updatedRows, people) =
      transactorBlock(result).unsafeRunSync()

    assert(insertedRowsUp == 1)
    assert(updatedRows == 1)
    assert(people == People(1, "Alice", 15))

    // Inserting and retrieving
    val david = transactorBlock(insert2("David", Some(27))).unsafeRunSync()
    assert(david == People(1, "David", 27))

    val insertedPeople =
      transactorBlock(insert2_H2(1, "Ramone", Option(42))).unsafeRunSync()
    assert(insertedPeople == People(1, "Ramone", 42))

    // Some rows to insert
    val peopleInfoData = List[PeopleInfo](("Frank", Some(12)), ("Daddy", None))
    val insertedRowsBatch =
      transactorBlock(insertMany(peopleInfoData)).unsafeRunSync()
    assert(insertedRowsBatch == 2)

    IO.pure[ExitCode](Success)
  }
}
