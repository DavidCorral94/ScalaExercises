package com.taglessfinal

import cats.Monad
import cats.effect.{ExitCode, IO, IOApp, Sync}
import com.taglessfinal.DataTypes.User
import com.taglessfinal.TypeClasses._

object DataTypes {
  case class Email(address: String)
  case class User(id: Int, name: String, emails: List[Email])
}

object TypeClasses {

  trait UserRepository[F[_]] {
    def findUser(id: Int): F[Option[User]]
  }

  object UserRepository {

    def apply[F[_]](implicit F: UserRepository[F]): UserRepository[F] = F

    implicit def instance[F[_]: Monad]: UserRepository[F] =
      new UserRepository[F] {
        override def findUser(id: Int): F[Option[User]] =
          Monad[F].pure(Option(User(id, "David", List())))
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
    case None => println("User not found")
  }

}
