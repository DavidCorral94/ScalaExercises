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
    def persistUser(user: User): F[Boolean]
    def findUser(id: Int): F[Option[User]]
    def updateUser(id: Int): F[Boolean]
  }

  object UserRepository {

    // Simulating a DB
    var db: Map[Int, User] = Map().empty

    def apply[F[_]](implicit F: UserRepository[F]): UserRepository[F] = F

    implicit def instance[F[_]: Monad]: UserRepository[F] =
      new UserRepository[F] {

        def persistUser(user: User): F[Boolean] = {
          db = db + (user.id -> user)
          Monad[F].pure(true)
        }

        def findUser(id: Int): F[Option[User]] =
          Monad[F].pure(Option(User(id, "David", List())))

        def addEmail(id: Int, email: Email ): F[Boolean] = ???
      }
  }
}

object Main extends App {

  val userIO = UserRepository[IO].findUser(1)
  val res = userIO.unsafeRunSync()
  res match {
    case None    => println("User not found")
    case Some(u) => println(u)
  }

  val userList = UserRepository[List].findUser(1)
  userList.foreach {
    case Some(u) => println(u)
    case None    => println("User not found")
  }

}
