
object Chapter7_1 {

  import scala.math.Ordering

  val minOrdering = Ordering.fromLessThan[Int](_ < _)

  val maxOrdering = Ordering.fromLessThan[Int](_ > _)

  List(3, 4, 2).sorted(minOrdering)
  List(3, 4, 2).sorted(maxOrdering)

  //implicit val ordering = Ordering.fromLessThan[Int](_ < _)
  List(3, 4, 2).sorted
  List(1, 7, 5).sorted

  // Ej 7.1.6.1
  /*
    val absOrdering = Ordering.fromLessThan[Int](Math.abs(_) < Math.abs(_))

    assert(List(-4, -1, 0, 2, 3).sorted(absOrdering) == List(0, -1, 2, 3, -4))
    assert(List(-4, -3, -2, -1).sorted(absOrdering) == List(-1, -2, -3, -4))

    implicit val ordering = absOrdering

    assert(List(-4, -1, 0, 2, 3).sorted == List(0, -1, 2, 3, -4))
    assert(List(-4, -3, -2, -1).sorted == List(-1, -2, -3, -4))
  */

  // Ej 7.1.6.2

  final case class Rational(numerator: Int, denominator: Int)

  implicit val ordering: Ordering[Rational] = Ordering.fromLessThan[Rational] {
    (a, b) =>
      (a.numerator.toDouble / a.denominator.toDouble) <
        (b.numerator.toDouble / b.denominator.toDouble)
  }

  assert(List(Rational(1, 2), Rational(3, 4), Rational(1, 3)).sorted ==
    List(Rational(1, 3), Rational(1, 2), Rational(3, 4)))

}

object Chapter7_2 {
  final case class Rational(numerator: Int, denominator: Int)

  object Rational {
    implicit val ordering = Ordering.fromLessThan[Rational]((x, y) =>
      (x.numerator.toDouble / x.denominator.toDouble) <
        (y.numerator.toDouble / y.denominator.toDouble)
    )
  }

  object Example {
    implicit val higherPriorityImplicit = Ordering.fromLessThan[Rational]((x, y) =>
      (x.numerator.toDouble / x.denominator.toDouble) >
        (y.numerator.toDouble / y.denominator.toDouble)
    )

    def example() =
      assert(List(Rational(1, 2), Rational(3, 4), Rational(1, 3)).sorted ==
        List(Rational(3, 4), Rational(1, 2), Rational(1, 3)))
  }

  Example.example()

  // Ej 7.2.5.1

  final case class Order(units: Int, unitPrice: Double) {
    val totalPrice: Double = units * unitPrice
  }

  object Order {
    implicit val ordering: Ordering[Order] =
      Ordering.fromLessThan[Order](_.totalPrice < _.totalPrice)
  }

  object OrderOrderedByUnits {
    implicit val ordering: Ordering[Order] =
      Ordering.fromLessThan[Order](_.units < _.units)
  }

  object OrderOrderedByUnitPrice {
    implicit val ordering: Ordering[Order] =
      Ordering.fromLessThan[Order](_.unitPrice < _.unitPrice)
  }

  val orders = List(Order(1, 2), Order(5, 4), Order(10, 1))

  import OrderOrderedByUnits.ordering

  orders.sorted
}

object Chapter7_3 {

  final case class Person(name: String, email: String)

  trait HtmlWriter[A] {
    def toHtml(in: A): String
  }

  object PersonWriter extends HtmlWriter[Person] {
    def toHtml(person: Person) =
      s"${person.name} (${person.email})"
  }

  object ObfuscatedPersonWriter extends HtmlWriter[Person] {
    def toHtml(person: Person) =
      s"${person.name} (${person.email.replaceAll("@", " at ")})"
  }

  // Ej 7.3.4.1

  trait Equal[A] {
    def equal(v1: A, v2: A): Boolean
  }

  object PersonEmailComparator extends Equal[Person] {
    def equal(p1: Person, p2: Person) = {
      p1.email == p2.email
    }
  }

  object PersonNameEmaiLComparator extends Equal[Person] {
    def equal(p1: Person, p2: Person) = {
      p1.email == p2.email && p1.name == p2.name
    }
  }
}

object Chapter7_4 {

  final case class Person(name: String, email: String)

  trait HtmlWriter[A] {
    def write(in: A): String
  }

  object HtmlUtil {
    def htmlify[A](data: A)(implicit writer: HtmlWriter[A]): String = {
      writer.write(data)
    }
  }

  implicit object ApproximationWriter extends HtmlWriter[Int] {
    def write(in: Int): String =
      s"It's definitely less than ${((in / 10) + 1) * 10}"
  }

  implicit object PersonWriter extends HtmlWriter[Person] {
    def write(person: Person) =
      s"${person.name} (${person.email})"
  }

  HtmlUtil.htmlify(10)

  HtmlUtil.htmlify(Person("David", "david.corral@47deg.com"))

  object HtmlWriter {
    def apply[A](implicit writer: HtmlWriter[A]): HtmlWriter[A] =
      writer
  }

  HtmlWriter[Person].write(Person("Noel", "noel@example.org"))

  // Ej 7.4.4.1

  trait Equal[A] {
    def equal(v1: A, v2: A): Boolean
  }

  object EmailEqual extends Equal[Person] {
    def equal(v1: Person, v2: Person): Boolean =
      v1.email == v2.email
  }

  object NameEmailEqual extends Equal[Person] {
    def equal(v1: Person, v2: Person): Boolean =
      v1.email == v2.email && v1.name == v2.name
  }

  object Equal {
    def apply[A](value1: A, value2: A)(implicit equal: Equal[A]) = {
      equal.equal(value1, value2)
    }

    def interface[A](implicit equal: Equal[A]): Equal[A] = {
      equal
    }
  }

  object NameAndEmailImplicit {
    implicit object NameEmailEqual extends Equal[Person] {
      def equal(v1: Person, v2: Person): Boolean =
        v1.email == v2.email && v1.name == v2.name
    }
  }

  object EmailImplicit {
    implicit object EmailEqual extends Equal[Person] {
      def equal(v1: Person, v2: Person): Boolean =
        v1.email == v2.email
    }
  }


  object Examples {
    def byNameAndEmail = {
      import NameAndEmailImplicit._
      Equal(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com"))(NameAndEmailImplicit.NameEmailEqual)
    }

    def byEmail = {
      import EmailImplicit._
      Equal(Person("Noel", "noel@example.com"), Person("Dave", "noel@example.com"))(EmailImplicit.EmailEqual)
    }

  }

  Examples.byNameAndEmail
  Examples.byEmail
}

object Chapter7_5 {
  implicit class ExtraStringMethods(str: String) {
    val vowels = Seq('a', 'e', 'i', 'o', 'u')

    def numberOfVowels =
      str.toList.filter(vowels contains _).length
  }

  new ExtraStringMethods("the quick brown fox").numberOfVowels

  "the quick brown fox".numberOfVowels

  implicit class ExtraIntegerMethods(int: Int) {
    def isEven: Boolean =
      int % 2 == 0
  }

  new ExtraIntegerMethods(4).isEven
  4.isEven

  final case class Person(name: String, email: String)

  trait HtmlWriter[A] {
    def toHtml(in: A): String
  }

  implicit object PersonWriter extends HtmlWriter[Person] {
    def toHtml(person: Person) =
      s"${person.name} (${person.email})"
  }

  implicit class HtmlOps[T](data: T) {
    def toHtml(implicit writer: HtmlWriter[T]) =
      writer.toHtml(data)
  }

  Person("John", "john@example.com").toHtml

  // Ej 7.6.2.1

  implicit class IntegerYeah(int: Int) {
    def yeah =
      times(_ => println("Oh yeah!"))

    // Ej 7.6.2.2
    def times(f: Int => Unit): Unit = {
      for (i <- 0 until int)
        f(i)
    }

    //   def yeah() = for{ _ <- 0 until n } println("Oh yeah!")
  }

  5.yeah
  (-1).yeah
  3.times(i => println(s"Look - it's the number $i!"))

  // Ej 7.6.2.3

  trait Equal[A] {
    def equal(v1: A, v2: A): Boolean
  }

  object Equal {
    def apply[A](implicit equal: Equal[A]): Equal[A] =
      equal

    implicit class ToEqual[A](in: A) {
      def ===(other: A)(implicit equal: Equal[A]): Boolean =
        equal.equal(in, other)
    }
  }

  implicit val caseInsensitiveEquals = new Equal[String] {
    def equal(s1: String, s2: String) =
      s1.toLowerCase == s2.toLowerCase
  }

  implicit val compareNumbers = new Equal[Int] {
    def equal(a1: Int, a2: Int) =
      a1 != a2
  }

  import Equal._

  "foo".===("FOO")

  5.===(2)

}

object Chapter7_8 {
  // Ej 7.8.3.1

  import scala.language.implicitConversions

  object IntImplicits {
    class IntOps(n: Int) {
      def yeah() =
        times(_ => println("Oh yeah!"))

      def times(func: Int => Unit) =
        for (i <- 0 until n) func(i)
    }

    implicit def intToIntOps(value: Int) =
      new IntOps(value)
  }

  import IntImplicits._

  5.yeah()
}

object Chapter7_9 {
  sealed trait JsValue {
    def stringify: String
  }

  final case class JsObject(values: Map[String, JsValue]) extends JsValue {
    def stringify = values
      .map { case (name, value) => "\"" + name + "\":" + value.stringify }
      .mkString("{", ",", "}")
  }

  final case class JsString(value: String) extends JsValue {
    def stringify = "\"" + value.replaceAll("\\|\"", "\\\\$1") + "\""
  }

  val obj = JsObject(Map("foo" -> JsString("a"), "bar" -> JsString("b"), "baz" -> JsString("c")))

  obj.stringify

  trait JsWriter[A] {
    def write(value: A): JsValue
  }

  object JsUtil {
    def toJson[A](value: A)(implicit writer: JsWriter[A]) =
      writer.write(value)
  }

  import java.util.Date

  sealed trait Visitor {
    def id: String

    def createdAt: Date

    def age: Long = new Date().getTime() - createdAt.getTime()
  }

  final case class Anonymous(
                              id: String,
                              createdAt: Date = new Date()
                            ) extends Visitor

  final case class User(
                         id: String,
                         email: String,
                         createdAt: Date = new Date()
                       ) extends Visitor

  implicit object AnonymousWriter extends JsWriter[Anonymous] {
    def write(value: Anonymous) =
      JsObject(Map(
        "id" -> JsString(value.id),
        "createdAt" -> JsString(value.createdAt.toString)
      ))
  }

  implicit object UserWriter extends JsWriter[User] {
    def write(value: User) = {
      JsObject(Map(
        "id" -> JsString(value.id),
        "email" -> JsString(value.email),
        "createdAt" -> JsString(value.createdAt.toString)
      ))
    }
  }

  implicit object VisitorWriter extends JsWriter[Visitor] {
    def write(value: Visitor) = value match {
      case anon: Anonymous => JsUtil.toJson(anon)
      case user: User => JsUtil.toJson(user)
    }
  }

  val visitors: Seq[Visitor] = Seq(Anonymous("001", new Date), User("003", "dave@xample.com", new Date))

  for {
    v <- visitors
  } println(JsUtil.toJson(v).stringify)

  // Ej 7.9.2
  /*
   implicit class ExtraIntegerMethods(int: Int) {
      def isEven: Boolean =
        int % 2 == 0
    }
   */
  implicit class JsUtil(json: JsValue) {
    def toJson =
      println(json.stringify)
  }

  for {
    v <- visitors
  } println(JsUtil.toJson(v).toJson)

}

object Chapter7_4v2 {
  // Ej 7.4.4.1

  case class Person(name: String, email: String)

  trait Equal[A] {
    def equal(v1: A, v2: A): Boolean
  }

  object EmailEqualImplicit {
    implicit object EmailEqual extends Equal[Person] {
      def equal(v1: Person, v2: Person): Boolean =
        v1.email == v2.email
    }
  }

  object NameAndEmailImplicit {
    implicit object NameEmailEqual extends Equal[Person] {
      def equal(v1: Person, v2: Person): Boolean =
        v1.email == v2.email && v1.name == v2.name
    }
  }

  object Eq {
    def apply[A](a1: A, a2: A)(implicit instance: Equal[A]): Boolean =
      instance.equal(a1, a2)
  }

  object Equal {
    def apply[A](implicit instance: Equal[A]): Equal[A] =
      instance
  }

  import NameAndEmailImplicit._

  Eq(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com"))

  import NameAndEmailImplicit._

  Equal[Person].equal(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com"))
}

object Chapter7_9v2 {
  sealed trait JsValue {
    def stringify: String
  }

  final case class JsObject(values: Map[String, JsValue]) extends JsValue {
    def stringify = values
      .map { case (name, value) => "\"" + name + "\":" + value.stringify }
      .mkString("{", ",", "}")
  }

  final case class JsString(value: String) extends JsValue {
    def stringify = "\"" + value.replaceAll("\\|\"", "\\\\$1") + "\""
  }

  sealed trait JsWriter[A] {
    def write(value: A): JsValue
  }

  object JsUtil {
    def toJson[A](value: A)(implicit writer: JsWriter[A]) = {
      writer.write(value)
    }
  }

  implicit class JsUtil[A](data: A) {
    def toJson(implicit writer: JsWriter[A]) =
      writer.write(data)
  }

  /** ********** */

  import java.util.Date

  sealed trait Visitor {
    def id: String

    def createdAt: Date

    def age: Long = new Date().getTime() - createdAt.getTime()
  }

  final case class Anonymous(
                              id: String,
                              createdAt: Date = new Date()
                            ) extends Visitor

  final case class User(
                         id: String,
                         email: String,
                         createdAt: Date = new Date()
                       ) extends Visitor

  /** ********** */

  implicit object AnonymousWriter extends JsWriter[Anonymous] {
    def write(value: Anonymous): JsValue = {
      JsObject(Map(
        "id" -> value.id.toJson,
        "createdAt" -> value.createdAt.toJson
      ))
    }
  }

  implicit object UserWriter extends JsWriter[User] {
    def write(value: User): JsValue = {
      JsObject(Map(
        "id" -> JsString(value.id),
        "email" -> JsString(value.email),
        "createdAt" -> JsString(value.createdAt.toString)
      ))
    }
  }

  implicit object VisitorWriter extends JsWriter[Visitor] {
    def write(value: Visitor): JsValue = value match {
      case anonymous: Anonymous => JsUtil.toJson(anonymous)
      case user: User => JsUtil.toJson(user)
    }
  }

  implicit object StringWriter extends JsWriter[String]{
    def write(value: String): JsValue =
      JsString(value)
  }

  implicit object DateWriter extends JsWriter[Date]{
    def write(value: Date): JsValue =
      JsString(value.toString)
  }

  Anonymous("001", new Date).toJson


}
