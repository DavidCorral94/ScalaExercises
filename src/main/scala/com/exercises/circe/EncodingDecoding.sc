import io.circe.syntax._
import io.circe.parser.decode

val intsJson = List(1, 2, 3).asJson

intsJson.as[List[Int]]

val decodeList = decode[List[Int]]("[1, 2, 3]")

decodeList.isRight
decodeList.getOrElse(Nil)

import io.circe._, io.circe.generic.semiauto._

case class Foo(a: Int, b: String, c: Boolean)
implicit val fooDecoder: Decoder[Foo] = deriveDecoder[Foo]
implicit val fooEncoder: Encoder[Foo] = deriveEncoder[Foo]

val jsonFoo = Foo(1, "Hello", true).asJson
jsonFoo.as[Foo]

import io.circe.generic.JsonCodec

/*
@JsonCodec case class Bar(i: Int, s: String)
val barJson = Bar(1, "Hello").asJson
barJson.as[Bar]
   */

case class User(id: Long, firstName: String, lastName: String)

object UserCodec {
  implicit val decodeUser: Decoder[User] =
    Decoder.forProduct3("id", "first_name", "last_name")(User.apply)

  implicit val encodeUser: Encoder[User] =
    Encoder.forProduct3("id", "first_name", "last_name")(u =>
      (u.id, u.firstName, u.lastName)
    )
}

case class Person(name: String)

object PersonCodec {
  implicit val decodePerson: Decoder[Person] =
    Decoder.forProduct1("name")(Person.apply)

  implicit val encodePerson: Encoder[Person] =
    Encoder.forProduct1("name")(p => (p.name))
}

case class Greeting(salutation: String, person: Person, exclamationMarks: Int)
import PersonCodec._

object GreetingCodec {

  implicit val decodeGreeting: Decoder[Greeting] =
    Decoder.forProduct3("salutation", "person", "exclamationMarks")(
      Greeting.apply
    )

  implicit val encodeGreeting: Encoder[Greeting] =
    Encoder.forProduct3("salutation", "person", "exclamationMarks")(g =>
      (g.salutation, g.person, g.exclamationMarks)
    )
}

import GreetingCodec._
val greetingJson = Greeting("Hey", Person("Chris"), 3).asJson

