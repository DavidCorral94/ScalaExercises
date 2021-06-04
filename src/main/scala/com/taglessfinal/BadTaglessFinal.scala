package com.taglessfinal

import cats.Monad
import cats.implicits._

import scala.language.implicitConversions

object BadTaglessFinal {

  // Source: https://www.baeldung.com/scala/tagless-final-pattern
  // But modified by guidance of Yago
  // Finished by a Type classes Masterclass of Rafa

  case class Address(address: String, city: String, country: String)
  case class User(id: Int, name: String, address: Address, emails: List[String])

  trait Users[F[_]] {
    def createUser(
        id: Int,
        name: String,
        address: Address,
        emails: List[String]
    ): F[Unit]
    def find(id: Int): F[Option[User]]
    def addEmail(user: User, email: String): F[User]
  }

  // Database mock
  // I know this is B.S. because in a real world we would have a DB connection service o something like that
  // but, at this point, this is the best (and easiest) way to represent a DB within my example
  var db: Map[Int, User] = Map().empty

  final class UsersProvider[F[_]: Monad] extends Users[F] {
    def createUser(
        id: Int,
        name: String,
        address: Address,
        emails: List[String]
    ): F[Unit] = {
      db = db + (id -> User(id, name, address, emails))
      Monad[F].unit
    }

    def find(id: Int): F[Option[User]] = {
      Monad[F].pure(db.get(id))
    }

    def addEmail(user: User, email: String): F[User] = {
      val emails = user.emails
      val newUser = user.copy(emails = email :: emails)
      Monad[F].pure(newUser)
    }
  }

  def main(args: Array[String]): Unit = {

    new UsersProvider[Option].createUser(
      1,
      "David",
      Address("Real 40", "San Fernando", "Spain"),
      List()
    )

    val user = new UsersProvider[Option].find(1)

    user.flatten match {
      case Some(x) => println(x.name)
      case None    => println("User not found")
    }
  }
}
