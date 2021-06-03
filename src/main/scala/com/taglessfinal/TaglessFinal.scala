package com.taglessfinal

import cats.Monad
import cats.data.State
import cats.implicits._
import com.taglessfinal.TaglessFinal.Program.createUserAndAddEmail


object TaglessFinal {

  // Source: https://www.baeldung.com/scala/tagless-final-pattern

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

  // Interpreter
  implicit object TestUserInterpreter extends Users[UserRepoState] {
    override def createUser(
        id: Int,
        name: String,
        address: Address,
        emails: List[String]
    ): UserRepoState[Unit] =
      State.modify { users =>
        val user = User(id, name, address, emails)
        users + (id -> user)
      }

    override def find(id: Int): UserRepoState[Option[User]] =
      State.inspect { users =>
        users.get(id)
      }

    override def addEmail(user: User, email: String): UserRepoState[User] =
      State { users =>
        val emails = user.emails
        val newUser = user.copy(emails = email :: emails)
        (users + (user.id -> newUser), newUser)
      }
  }

  // Program
  object Program {

    def createUser(
        id: Int,
        name: String,
        address: Address
    ): UserRepoState[Unit] =
      TestUserInterpreter.createUser(id, name, address, List())

    def createUserAndAddEmail[F[_]: Monad](
        id: Int,
        name: String,
        address: Address,
        email: String
    )(implicit users: Users[F]): F[Option[User]] = {
      for {
        _ <- users.createUser(id, name, address, List())
        insertedUser <- users.find(id)
        userWithEmail <- insertedUser.traverse(u => users.addEmail(u, email))
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
