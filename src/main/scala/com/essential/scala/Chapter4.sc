object Chapter4_1 {
  /**
   * import java.util.Date
   *
   * case class Anonymous(id: String, createdAt: Date = new Date())
   *
   * case class User(id: String, email: String, createdAt: Date = new Date())
   */

  import java.util.Date

  trait Visitor {
    def id: String // Unique id assigned to each user

    def createdAt: Date // Date this user first visited the site

    // How long has this visitor been around?
    def age: Long = new Date().getTime - createdAt.getTime
  }

  case class Anonymous(id: String, createdAt: Date = new Date()) extends Visitor

  case class User(id: String, email: String, createdAt: Date = new Date()) extends Visitor

  case object EmailService {
    def email(SMTP: String, MIME: String, stuff: String, visitor: Visitor): String =
      visitor match {
        case Anonymous(_, _) => s"Can't send email to anon"
        case User(_, email, _) => s"Sending email to $email"
      }
  }

  EmailService.email("test", "test", "stuff", new User("111", "david.corral@47deg.com"))

  def older(v1: Visitor, v2: Visitor): Boolean =
    v1.createdAt.before(v2.createdAt)

  older(Anonymous("1"), User("2", "test@example.com"))
  // res5: Boolean = true

  // Ej 4.1.4.1
  trait Feline {
    def colour: String

    def sound: String = "roar"
  }

  case class Cat(colour: String, food: String, override val sound: String = "meow") extends Feline

  case class Lion(colour: String, maneSize: Int) extends Feline

  case class Tiger(colour: String) extends Feline

  case class Panther(colour: String) extends Feline

  Cat("orange", "tuna").colour
  Lion("yellow", 1).colour
  Tiger("orange&black").colour
  Panther("black").colour

  // Ej 4.1.4.2

  trait Shape {
    def sides: Int

    def perimeter: Double

    def area: Double
  }

  case class Circle(radius: Double) extends Shape {
    val sides = 1
    val perimeter = 2 * Math.PI * radius
    val area = Math.PI * Math.pow(radius, 2)
  }

  case class Square(width: Double) extends Shape {
    val sides = 4
    val perimeter = sides * width

    val area = Math.pow(width, 2)
  }

  case class Rectangle(width: Double, height: Double) extends Shape {
    val sides = 4
    val perimeter = width + width + height + height

    val area = width * height
  }

  Square(10.0).area
  Square(10.0).perimeter

  Rectangle(10.0, 5.0).area
  Rectangle(10.0, 5.0).perimeter

  Circle(3.0).area
  Circle(3.0).perimeter

  // Ej 4.1.4.3

  sealed trait Rectangular extends Shape {
    def width: Double

    def height: Double

    val sides: Int = 4

    override val perimeter = 2 * width + 2 * height
    override val area = width * height
  }

  case class SquareR(size: Double) extends Rectangular {
    val width = size
    val height = size
  }

  case class RectangleR(width: Double, height: Double) extends Rectangular

  SquareR(5).width
  SquareR(5).height
  SquareR(5).area

  RectangleR(4, 5).area
  RectangleR(4, 5).perimeter
}

object Chapter4_2 {

  import java.util.Date

  sealed trait Visitor {
    def id: String

    def createdAt: Date

    def age: Long = new Date().getTime() - createdAt.getTime()
  }

  final case class User(id: String, createdAt: Date, email: String) extends Visitor

  final case class Anonymous(id: String, createdAt: Date = new Date()) extends Visitor


  // Ej 4.2.2.1
  /*
    sealed trait Shape {
      def sides: Int

      def perimeter: Double

      def area: Double
    }

    final case class Circle(radius: Double) extends Shape {
      val sides = 1
      val perimeter = 2 * Math.PI * radius
      val area = Math.PI * Math.pow(radius, 2)
    }

    final case class Square(width: Double) extends Shape {
      val sides = 4
      val perimeter = sides * width

      val area = Math.pow(width, 2)
    }

    final case class Rectangle(width: Double, height: Double) extends Shape {
      val sides = 4
      val perimeter = width + width + height + height

      val area = width * height
    }

    // Compiler doesn't complains when comment a case even if
    // it is a sealed trait :S
    object Draw {
      def apply(shape: Shape): String = {
        shape match {
          case Circle(radius) => s"A circle of radius $radius"
          case Rectangle(width, height) => s"A rectangle of $width * $height"
          case Square(width) => s"A square of $width"
        }
      }
    }

    Draw(Circle(10))
    Draw(Rectangle(3, 4))
    Draw(Square(1))

    */

  // Ej 4.2.2.2

  sealed trait Color {
    def red: Int

    def green: Int

    def blue: Int

    def isLight = (red + green + blue) / 3.0 > 0.5

    def isDark = !isLight
  }

  final case object Red extends Color {
    val red = 255
    val green = 0
    val blue = 0
  }

  final case object Pink extends Color {
    val red = 255
    val green = 182
    val blue = 193
  }

  final case object Yellow extends Color {
    val red = 239
    val green = 239
    val blue = 0
  }

  final case class CustomColor(red: Int, green: Int, blue: Int) extends Color

  sealed trait Shape {
    def sides: Int

    def perimeter: Double

    def area: Double

    def color: Color
  }

  final case class Circle(radius: Double, color: Color) extends Shape {
    val sides = 1
    val perimeter = 2 * Math.PI * radius
    val area = Math.PI * Math.pow(radius, 2)
  }

  sealed trait Rectangular extends Shape {
    def width: Double

    def height: Double

    val sides = 4
    val perimeter = 2 * width + 2 * height
    val area = width * height
  }

  final case class Square(size: Double, color: Color) extends Rectangular {
    val width = size
    val height = size
  }

  final case class Rectangle(width: Double, height: Double, color: Color) extends Rectangular


  object Draw {
    def apply(shape: Shape): String = shape match {
      case Circle(radius, color) =>
        s"A ${Draw(color)} circle of radius ${radius}cm"

      case Square(size, color) =>
        s"A ${Draw(color)} square of size ${size}cm"

      case Rectangle(width, height, color) =>
        s"A ${Draw(color)} rectangle of width ${width}cm and height ${height}cm"
    }

    def apply(color: Color): String = color match {
      // We deal with each of the predefined Colors with special cases:
      case Red => "red"
      case Yellow => "yellow"
      case Pink => "pink"
      case color => if (color.isLight) "light" else "dark"
    }
  }

  Draw(Circle(10, Pink))

  Draw(Rectangle(3, 4, CustomColor(4, 4, 4)))

  // Ej 4.2.2.3

  sealed trait DivisionResult

  final case class Finite(a: Int) extends DivisionResult

  // This can be a object extending DivisionResult because it
  // doesn't need any params
  final case class Infinite() extends DivisionResult

  case object divide {
    def apply(a: Int, b: Int): DivisionResult = {
      b match {
        case 0 => Infinite()
        case _ => Finite(a / b)
      }
    }
  }

  val x = divide(1, 2)
  // x: DivisionResult = Finite(0)

  val y = divide(1, 0)
  // y: DivisionResult = Infinite

  divide(1, 0) match {
    case Finite(a) => s"It's finite: ${a}"
    case Infinite() => s"It's infinite"
  }

  divide(4, 2) match {
    case Finite(a) => s"It's finite: ${a}"
    case Infinite() => s"It's infinite"
  }
}

object Chapter4_4 {
  // Ej 4.4.4.1
  sealed trait TrafficLight

  final case object Red extends TrafficLight

  final case object Green extends TrafficLight

  final case object Yellow extends TrafficLight

  // Ej 4.4.4.2
  sealed trait Calculation

  final case class Success(result: Int) extends Calculation

  final case class Failure(reason: String) extends Calculation

  // Ej 4.4.4.3
  sealed trait Source

  case object Well extends Source

  case object Spring extends Source

  case object Tap extends Source

  final case class BottledWater(size: Int, source: Source, carbonated: Boolean)

}

object Chapter4_5 {
  sealed trait A {
    def foo: String = "It's A!"
  }

  final case class B() extends A {
    override def foo: String =
      "It's B!"
  }

  final case class C() extends A {
    override def foo: String =
      "It's C!"
  }

  val anA: A = B()
  anA.foo

  val anAv2: A = C()
  anAv2.foo


  sealed trait Feline {
    def dinner: Food = {
      this match {
        case Lion() => Antelope
        case Tiger() => TigerFood
        case Panther() => Licorice
        case Cat(food) => CatFood(food)
      }
    }
  }

  final case class Lion() extends Feline

  final case class Tiger() extends Feline

  final case class Panther() extends Feline

  final case class Cat(favouriteFood: String) extends Feline

  sealed trait Food

  case object Antelope extends Food

  case object TigerFood extends Food

  case object Licorice extends Food

  final case class CatFood(food: String) extends Food

  // Ej 4.5.6.1
  sealed trait TrafficLight {
    def next: TrafficLight =
      this match {
        case Red => Green
        case Green => Yellow
        case Yellow => Red
      }
  }

  case object Red extends TrafficLight

  case object Green extends TrafficLight

  case object Yellow extends TrafficLight


  // Ej 4.5.6.2
  sealed trait Calculation

  final case class Success(result: Int) extends Calculation

  final case class Failure(reason: String) extends Calculation

  case object Calculator {
    def +(c: Calculation, x: Int): Calculation =
      c match {
        case Success(result) => Success(result + x)
        case Failure(reason) => Failure(reason)
      }

    def -(c: Calculation, x: Int): Calculation =
      c match {
        case Success(result) => Success(result - x)
        case Failure(reason) => Failure(reason)
      }

    def /(c: Calculation, x: Int): Calculation =
      c match {
        case Success(result) =>
          x match {
            case 0 => Failure("Division by zero")
            case _ => Success(result / x)
          }
        case Failure(result) => Failure(result)
      }
  }

  assert(Calculator.+(Success(1), 1) == Success(2))
  assert(Calculator.-(Success(1), 1) == Success(0))
  assert(Calculator.+(Failure("Badness"), 1) == Failure("Badness"))

  assert(Calculator./(Success(4), 2) == Success(2))
  assert(Calculator./(Success(4), 0) == Failure("Division by zero"))
  assert(Calculator./(Failure("Badness"), 0) == Failure("Badness"))
}

object Chapter4_6 {
  /*sealed trait IntList

  case object End extends IntList

  final case class Pair(head: Int, tail: IntList) extends IntList

  val example = Pair(1, Pair(2, Pair(3, End)))
  assert(sum(example) == 6)
  assert(sum(example.tail) == 5)
  assert(sum(End) == 0)

  def sum(list: IntList): Int = {
    list match {
      case End => 0
      case Pair(head, tail) => head + sum(tail)
    }
  }*/

  /* FAILS
    import scala.annotation.tailrec
    @tailrec
    def sum(list: IntList): Int =
      list match {
        case End => 0
        case Pair(hd, tl) => sum(tl)
      }
  */

  import scala.annotation.tailrec

  @tailrec
  def sum(list: IntList, total: Int = 0): Int =
    list match {
      case End => total
      case Pair(hd, tl) => sum(tl, total + hd)
    }

  // Ej 4.6.3.1

  sealed trait IntList {
    def length: Int = {
      this match {
        case End => 0
        case Pair(head, tail) => 1 + tail.length
      }
    }

    def product: Int = {
      this match {
        case End => 1
        case Pair(head, tail) => head * tail.product
      }
    }

    def double: IntList = {
      this match {
        case End => End
        case Pair(head, tail) => Pair(head * 2, tail.double)
      }
    }
  }

  case object End extends IntList

  final case class Pair(head: Int, tail: IntList) extends IntList

  val example = Pair(1, Pair(2, Pair(3, End)))

  assert(example.length == 3)
  assert(example.tail.length == 2)
  assert(End.length == 0)

  assert(example.product == 6)
  assert(example.tail.product == 6)
  assert(End.product == 1)

  assert(example.double == Pair(2, Pair(4, Pair(6, End))))
  assert(example.tail.double == Pair(4, Pair(6, End)))
  assert(End.double == End)

  // Ej 4.6.3.2
  sealed trait Tree {
    def sumPol: Int

    def doublePol: Tree

    def sum: Int = {
      this match {
        case Leaf(value) => value
        case Node(l, r) => l.sum + r.sum
      }
    }

    def double: Tree = {
      this match {
        case Leaf(value) => Leaf(value * 2)
        case Node(l, r) => Node(l.double, r.double)
      }
    }
  }

  final case class Node(l: Tree, r: Tree) extends Tree {
    def sumPol: Int = l.sum + r.sum

    def doublePol: Tree = Node(l.double, r.double)

  }

  final case class Leaf(value: Int) extends Tree {
    def sumPol: Int = value

    def doublePol: Tree = Leaf(value * 2)
  }
}

object Chapter4_7 {

  // Ej 4.7.0.1

  sealed trait Expression {
    def eval: Calculation = {
      this match {
        case Addition(l, r) =>
          l.eval match {
            case Failure(reason) => Failure(reason)
            case Success(r1) =>
              r.eval match {
                case Failure(reason) => Failure(reason)
                case Success(r2) => Success(r1 + r2)
              }
          }
        case Subtraction(l, r) =>
          l.eval match {
            case Failure(reason) => Failure(reason)
            case Success(r1) => {
              r.eval match {
                case Failure(reason) => Failure(reason)
                case Success(r2) => Success(r1 + r2)
              }
            }
          }
        case Division(l, r) =>
          l.eval match {
            case Failure(reason) => Failure(reason)
            case Success(r1) => {
              r.eval match {
                case Failure(reason) => Failure(reason)
                case Success(r2) => {
                  if (r2 == 0)
                    Failure("Division by zero")
                  else
                    Success(r1 / r2)
                }
              }
            }
          }
        case SquareRoot(v) =>
          v.eval match {
            case Success(r) => {
              if (r < 0)
                Failure("Square root of negative number")
              else
                Success(Math.sqrt(r))
            }
            case Failure(reason) => Failure(reason)
          }
        case Number(v) => Success(v)
      }
    }
  }

  final case class Addition(l: Expression, r: Expression) extends Expression

  final case class Subtraction(l: Expression, r: Expression) extends Expression

  final case class Division(l: Expression, r: Expression) extends Expression

  final case class SquareRoot(value: Expression) extends Expression

  final case class Number(value: Double) extends Expression

  sealed trait Calculation

  final case class Success(result: Double) extends Calculation

  final case class Failure(reason: String) extends Calculation

  assert(Addition(SquareRoot(Number(-1.0)), Number(2.0)).eval ==
    Failure("Square root of negative number"))
  assert(Addition(SquareRoot(Number(4.0)), Number(2.0)).eval == Success(4.0))
  assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))

  // Ej 4.7.0.2
  /*
    Json ::= JsNumber value:Double
         | JsString value:String
         | JsBoolean value:Boolean
         | JsNull
         | JsSequence
         | JsObject
    JsSequence ::= SeqCell head:Json tail:JsSequence
                 | SeqEnd
    JsObject ::= ObjectCell key:String value:Json tail:JsObject
               | ObjectEnd
   */

  sealed trait Json {
    def print: String = {
      def quote(s: String): String =
        '"'.toString ++ s ++ '"'.toString

      def seqToJson(seq: SeqCell): String =
        seq match {
          case SeqCell(h, t@SeqCell(_, _)) =>
            s"${h.print}, ${seqToJson(t)}"
          case SeqCell(h, SeqEnd) => h.print
        }

      def objectToJson(obj: ObjectCell): String =
        obj match {
          case ObjectCell(k, v, t@ObjectCell(_, _, _)) =>
            s"${quote(k)}: ${v.print}, ${objectToJson(t)}"
          case ObjectCell(k, v, ObjectEnd) =>
            s"${quote(k)}: ${v.print}"
        }

      this match {
        case JsNumber(v) => v.toString
        case JsString(v) => quote(v)
        case JsBoolean(v) => v.toString
        case JsNull => "null"
        case s@SeqCell(_, _) => "[" ++ seqToJson(s) ++ "]"
        case SeqEnd => "[]"
        case o@ObjectCell(_, _, _) => "{" ++ objectToJson(o) ++ "}"
        case ObjectEnd => "{}"
      }
    }
  }

  final case class JsNumber(value: Double) extends Json

  final case class JsString(value: String) extends Json

  final case class JsBoolean(value: Boolean) extends Json

  final case object JsNull extends Json

  trait JsSequence extends Json

  final case class SeqCell(head: Json, tail: JsSequence) extends JsSequence

  final case object SeqEnd extends JsSequence

  trait JsObject extends Json

  final case class ObjectCell(key: String, value: Json, tail: JsObject) extends JsObject

  final case object ObjectEnd extends JsObject

  SeqCell(JsString("a string"), SeqCell(JsNumber(1.0), SeqCell(JsBoolean(true), SeqEnd))).print

  ObjectCell(
    "a", SeqCell(JsNumber(1.0), SeqCell(JsNumber(2.0), SeqCell(JsNumber(3.0), SeqEnd))),
    ObjectCell(
      "b", SeqCell(JsString("a"), SeqCell(JsString("b"), SeqCell(JsString("c"), SeqEnd))),
      ObjectCell(
        "c", ObjectCell("doh", JsBoolean(true),
          ObjectCell("ray", JsBoolean(false),
            ObjectCell("me", JsNumber(1.0), ObjectEnd))),
        ObjectEnd
      )
    )
  ).print

}