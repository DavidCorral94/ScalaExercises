package com.taglessfinal

import cats.Monad
import cats.data.State
import cats.effect.IO
import cats.implicits._
import com.taglessfinal.TaglessFinal.Program.createUserAndAddEmail

object TaglessFinal {

  // Source: https://www.baeldung.com/scala/tagless-final-pattern
  // But modified by guidance of Yago

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
  type UserRepository = Map[Int, User]
  type UserRepoState[A] = State[UserRepository, A]

  object Users {
    def apply[F[_]](implicit ev: Users[F]): Users[F] = ev

    def impl[F[_]: Monad](db: UserRepository): Users[F] =
      new Users[F] {
        def createUser(
            id: Int,
            name: String,
            address: Address,
            emails: List[String]
        ): F[Unit] = {
          // Aquí he de actualizar la BBDD y devolver F[Unit]
          db + (id -> User(id, name, address, emails))
          Monad[F].unit
        }

        def find(id: Int): F[Option[User]] = {
          // Aquí he de consultar l BBDD y devolver F[Option[User]
          Monad[F].pure(db.get(id))
        }

        def addEmail(user: User, email: String): F[User] = {
          val emails = user.emails
          val newUser = user.copy(emails = email :: emails)
          Monad[F].pure(newUser)
        }
      }
  }

  // Program
  object Program {
    def createUser[F[_]: Monad: Users](
        id: Int,
        name: String,
        address: Address
    ): F[Unit] =
      Users[F].createUser(id, name, address, List())

    def findUser[F[_]: Monad: Users](id: Int): F[Option[User]] =
      Users[F].find(id)

    def createUserAndAddEmail[F[_]: Monad: Users](
        id: Int,
        name: String,
        address: Address,
        email: String
    ): F[Option[User]] = {
      for {
        _ <- Users[F].createUser(id, name, address, List())
        insertedUser <- Users[F].find(id)
        userWithEmail <- insertedUser.traverse(u => Users[F].addEmail(u, email))
      } yield userWithEmail
    }
  }

  def main(args: Array[String]): Unit = {
    val dbState = createUserAndAddEmail[UserRepoState](
      1,
      "David",
      Address("Real 40", "San Fernando", "Spain"),
      "david.corral@47deg.com"
    )

    val (_, user) = dbState.run(Map()).value
    println(user.get.emails.contains("david.corral@47deg.com"))
  }
}
