package com.taglessfinal

import cats.Monad
import cats.effect.{ExitCode, IO, IOApp, Sync}
import com.taglessfinal.DataTypes.{Email, User}
import com.taglessfinal.TypeClasses._

object DataTypes {
  case class Email(address: String)
  case class User(id: Int, name: String, emails: List[Email])
}

object TypeClasses {

  trait UserRepository[F[_]] {
    def listUsers(): F[List[User]]
    def persistUser(user: User): F[Boolean]
    def findUser(id: Int): F[Option[User]]
    def addEmail(user: User, email: Email): F[Boolean]
  }

  object UserRepository {

    // Simulating a DB
    var db: Map[Int, User] = Map().empty

    def apply[F[_]](implicit F: UserRepository[F]): UserRepository[F] = F

    implicit def instance[F[_]: Monad]: UserRepository[F] =
      new UserRepository[F] {

        def listUsers(): F[List[User]] =
          Monad[F].pure(db.values.toList)

        def persistUser(user: User): F[Boolean] = {
          db = db + (user.id -> user)
          Monad[F].pure(true)
        }

        def findUser(id: Int): F[Option[User]] =
          if (db.contains(id))
            Monad[F].pure(db.get(id))
          else
            Monad[F].pure(None)

        def addEmail(user: User, email: Email): F[Boolean] = {
          val newUser = user.copy(emails = email :: user.emails)
          db = db + (user.id -> newUser)
          Monad[F].pure(true)
        }
      }
  }
}

object Main extends App {

  val user = User(1, "David Corral", List())
  val userAdd = UserRepository[IO].persistUser(user)
  println(s" User added? ${userAdd.unsafeRunSync()}")
  val maybeUser = UserRepository[IO].findUser(1)
  println(s" User ID 1 = ${maybeUser.unsafeRunSync()}")

  val res = for {
    maybeU <- UserRepository[IO].findUser(1)
    updatedUser <- maybeU match {
      case Some(x) =>
        UserRepository[IO].addEmail(x, Email("david.corral@47deg.com"))
      case None =>
        throw new Exception("Oops, user not found")
    }

  } yield updatedUser

  println(s" User updated? ${res.unsafeRunSync()}")

  val users = UserRepository[IO].listUsers().unsafeRunSync()
  println(users)

}
