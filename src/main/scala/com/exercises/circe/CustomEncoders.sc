import io.circe._
import io.circe.syntax._

case class Foo(value: String)

implicit val fooKeyEncoder = new KeyEncoder[Foo] {
  override def apply(foo: Foo): String = foo.value
}

val map = Map[Foo, Int](
  Foo("hello") -> 123,
  Foo("world") -> 456
)

val json = map.asJson

implicit val fooKeyDecoder = new KeyDecoder[Foo] {
  override def apply(key: String): Option[Foo] = Some(Foo(key))
}

json.as[Map[Foo, Int]]


