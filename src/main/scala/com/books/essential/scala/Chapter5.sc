object Chapter5_1 {
  final case class Box[A](value: A)

  Box(2).value
  Box("hi").value

  def generic[A](in: A): A = in

  generic[String]("foo")
  generic(1)

  /**
   * sealed trait Calculation
   * final case class Success(result: Double) extends Calculation
   * final case class Failure(reason: String) extends Calculation
   */

  sealed trait Result[A]

  final case class Success[A](result: A) extends Result[A]

  final case class Failure[A](reason: String) extends Result[A]


  // Ej 5.1.3.1
  // Ej 5.1.3.2

  sealed trait LinkedList[A] {
    def length: Int = {
      this match {
        case End() => 0
        case Pair(head, tail) => 1 + tail.length
      }
    }

    def contains(el: A): Boolean = {
      this match {
        case End() => false
        case Pair(head, tail) => {
          if (head == el)
            true
          else
            tail.contains(el)
        }
      }
    }

    def apply(n: Int): Result[A] = {
      this match {
        case End() => Failure("Index out of bounds")
        case Pair(head, tail) =>
          if (n == 0)
            Success(head)
          else
            tail.apply(n - 1)
      }
    }
  }

  final case class End[A]() extends LinkedList[A]

  final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]


  val example = Pair(1, Pair(2, Pair(3, End())))
  assert(example.length == 3)
  assert(example.tail.length == 2)
  assert(End().length == 0)

  assert(example.contains(3))
  assert(!example.contains(4))
  assert(!End().contains(0))

  assert(example(0) == Success(1))
  assert(example(1) == Success(2))
  assert(example(2) == Success(3))
  assert(example(3) == Failure("Index out of bounds"))
}

object Chapter5_2 {
  /*
  sealed trait IntList {
    def abstraction(end: Int, f: ???): Int =
      this match {
        case End => end
        case Pair(hd, tl) => f(hd, tl.abstraction(end, f))
      }
  }

  case object End extends IntList

  case class Pair(hd: Int, tl: IntList) extends IntList
  */

  val sayHi = () => "Hi!"

  sayHi()

  val add1 = (x: Int) => x + 1

  add1(10)

  val sum = (x: Int, y: Int) => x + y

  sum(10, 20)

  // Ej 5.2.3.1

  sealed trait IntList {
    def fold[A](end: A, f: (Int, A) => A): A =
      this match {
        case End => end
        case Pair(head, tail) => f(head, tail.fold(end, f))
      }

    def sum(): Int =
      this.fold[Int](0, (head, tail) => head + tail)

    def product(): Int =
      this.fold[Int](1, (head, tail) => head * tail)

    def length(): Int =
      this.fold[Int](0, (_, tail) => 1 + tail)

    def double(): IntList =
      this.fold[IntList](End, (head, tail) => Pair(head * 2, tail))
  }

  case object End extends IntList

  final case class Pair(head: Int, tail: IntList) extends IntList


  /**
   * // Ej 5.2.3.1
   * /*
   * sealed trait IntList {
   * def fold(end: Int, f: (Int, Int) => Int): Int =
   * this match {
   * case End => end
   * case Pair(hd, tl) => f(hd, tl.fold(end, f))
   * }
   *
   * def sum: Int = fold(0, (hd, tl) => hd + tl)
   *
   * def product: Int = fold(0, (hd, tl) => hdtl)
   *
   * def length: Int = fold(0, (_, tl) => 1 + tl)
   * }
   *
   * case object End extends IntList
   *
   * final case class Pair(head: Int, tail: IntList) extends IntList
   * */
   * // Generalised version
   * sealed trait IntList {
   * def fold[A](end: A, f: (Int, A) => A): A =
   * this match {
   * case End => end
   * case Pair(hd, tl) => f(hd, tl.fold(end, f))
   * }
   *
   * def sum: Int = fold[Int](0, (hd, tl) => hd + tl)
   *
   * def product: Int = fold[Int](0, (hd, tl) => hdtl)
   *
   * def length: Int = fold[Int](0, (_, tl) => 1 + tl)
   *
   * def double: IntList = fold[IntList](End, (hd, tl) => Pair(2hd, tl))
   * }
   *
   * case object End extends IntList
   *
   * final case class Pair(head: Int, tail: IntList) extends IntList
   */
}

object Chapter5_3_v2 {
  sealed trait LinkedList[A] {
    def fold[B](end: B, pair: (A, B) => B): B = {
      this match {
        case End() => end
        case Pair(hd, tl) => pair(hd, tl.fold(end, pair))
      }
    }
  }

  final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

  final case class End[A]() extends LinkedList[A]

  // If leaf received no values
  sealed trait Tree[A] {
    def fold[B](leaf: B, node: (B, B) => B): B = {
      this match {
        case Leaf() => leaf
        case Node(left, right) => node(left.fold(leaf, node), right.fold(leaf, node))
      }
    }
  }

  final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A]() extends Tree[A]
  /*
    val tree: Tree[String] =
      Node(Node(Leaf("To"), Leaf("iterate")),
        Node(Node(Leaf("is"), Leaf("human,")),
          Node(Leaf("to"), Node(Leaf("recurse"), Leaf("divine")))))

    tree.fold[String](str => str, (a, b) => a + " " + b)*/
}


object Chapter5_3 {
  sealed trait LinkedList[A] {
    def fold[B](end: B, pair: (A, B) => B): B =
      this match {
        case End() => end
        case Pair(hd, tl) => pair(hd, tl.fold(end, pair))
      }
  }

  final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

  final case class End[A]() extends LinkedList[A]

  // Ej 5.3.4.1
  sealed trait Tree[A] {
    def fold[B](node: (B, B) => B, leaf: A => B): B /*= {
      this match {
        case Leaf(el) => leaf(el)
        case Node(l, r) => node(l.fold(node, leaf), r.fold(node, leaf))
      }
    }*/
  }

  final case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A] {
    def fold[B](node: (B, B) => B, leaf: A => B): B =
      node(l.fold(node, leaf), r.fold(node, leaf))

  }

  final case class Leaf[A](el: A) extends Tree[A] {
    def fold[B](node: (B, B) => B, leaf: A => B): B =
      leaf(el)
  }

  val tree: Tree[String] =
    Node(
      Node(Leaf("To"), Leaf("iterate")),
      Node(Node(Leaf("is"), Leaf("human,")),
        Node(Leaf("to"),
          Node(Leaf("recurse"), Leaf("divine")))))


  tree.fold[String]((a, b) => a + " " + b, str => str)

}

object Chapter5_4v2 {
  sealed trait Sum[A, B] {
    def fold[C](left: A => C, right: B => C): C = {
      this match {
        case Left(value) => left(value)
        case Right(value) => right(value)
      }
    }
  }

  case class Left[A, B](value: A) extends Sum[A, B]

  case class Right[A, B](value: B) extends Sum[A, B]

  Left[Int, String](1).value
  // res9: Int = 1

  Right[Int, String]("foo").value
  // res10: String = foo

  val sum: Sum[Int, String] = Right("foo")
  // sum: sum.Sum[Int,String] = Right(foo)

  sum match {
    case Left(x) => x.toString
    case Right(x) => x
  }

  sealed trait Maybe[A] {
    def fold[B](empty: B, full: A => B): B = {
      this match {
        case Empty() => empty
        case Full(value) => full(value)
      }
    }
  }

  case class Full[A](value: A) extends Maybe[A]

  final case class Empty[A]() extends Maybe[A]
}


object Chapter5_4 {
  case class Pair[A, B](a: A, b: B)

  def intAndString: Pair[Int, String] = Pair(2, "hi")

  def booleanAndDouble: Pair[Boolean, Double] = Pair(true, 2.0)

  // Ej 5.4.1.1
  intAndString.a
  booleanAndDouble.b

  def tuplized[A, B, C](in: (A, B, C)) = in._3

  tuplized(("aa", 1, true))

  //Ej 5.4.3.1
  /*
  sealed trait Sum[A, B]
   *
  case class Left[A, B](value: A) extends Sum[A, B]
   *
  case class Right[A, B](value: B) extends Sum[A, B]
   *
  def intOrString(input: Boolean): Sum[Int, String] =
  if (input) {
  Left[Int, String](123)
  } else {
  Right[Int, String]("abc")
  }
   *
  Left[Int, String](1).value
  Right[Int, String]("foo").value
  val sum: Sum[Int, String] = Right("foo")
  sum match {
  case Left(x) => x.toString
  case Right(x) => x
  }
  */
  // Ej 5.4.4.1
  // Ej 5.4.6.2
  sealed trait Maybe[A] {
    def fold[B](full: A => B, empty: B): B = {
      this match {
        case Empty() => empty
        case Full(v) => full(v)
      }
    }
  }

  final case class Full[A](value: A) extends Maybe[A]

  final case class Empty[A]() extends Maybe[A]

  val perhaps: Maybe[Int] = Empty[Int]

  val perhaps2: Maybe[Int] = Full(1)

  // Ej 5.4.6.3
  sealed trait Sum[A, B] {
    def fold[C](left: A => C, right: B => C): C = {
      this match {
        case Left(a) => left(a)
        case Right(b) => right(b)
      }
    }

  }

  final case class Left[A, B](value: A) extends Sum[A, B]

  final case class Right[A, B](value: B) extends Sum[A, B]

}

object Chapter5_5v2 {
  sealed trait LinkedList[A] {
    def map[B](fn: A => B): LinkedList[B] =
      this match {
        case Pair(hd, tl) => Pair(fn(hd), tl.map(fn))
        case End() => End[B]()
      }
  }

  final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

  final case class End[A]() extends LinkedList[A]

  sealed trait Maybe[A] {
    def map[B](fn: A => B): Maybe[B] = {
      this match {
        case Full(value) => Full(fn(value))
        case Empty() => Empty[B]()
      }
    }

    def flatMap[B](fn: A => Maybe[B]): Maybe[B] = {
      this match {
        case Full(value) => fn(value)
        case Empty() => Empty[B]()
      }
    }

    def mapFlat[B](fn: A => B): Maybe[B] =
      flatMap[B](value => Full(fn(value)))
  }

  final case class Full[A](value: A) extends Maybe[A]

  final case class Empty[A]() extends Maybe[A]

  val list: LinkedList[Int] = Pair(1, Pair(2, Pair(3, End())))

  list.map(_ * 2)
  list.map(l => l + 1)
  list.map(l => l / 3)


  val listv2 = List(1, 2, 3)

  listv2.flatMap(l => List(l, -l))

  val listv3: List[Maybe[Int]] = List(Full(3), Full(2), Full(1))

  listv3.map(el => {
    el.flatMap[Int] { x =>
      if (x % 2 == 0)
        Full(x)
      else
        Empty()
    }
  })

  listv3.map(maybe => maybe.flatMap[Int] { x => if (x % 2 == 0) Full(x) else Empty() })


  sealed trait Sum[A, B] {
    def fold[C](failure: A => C, success: B => C): C = {
      this match {
        case Failure(reason) => failure(reason)
        case Success(value) => success(value)
      }
    }

    def map[C](fn: B => C): Sum[A, C] = {
      this match {
        case Failure(reason) => Failure(reason)
        case Success(value) => Success(fn(value))
      }
    }

    def flatMap[C](fn: B => Sum[A, C]): Sum[A, C] = {
      this match {
        case Failure(reason) => Failure(reason)
        case Success(value) => fn(value)
      }
    }
  }

  final case class Failure[A, B](reason: A) extends Sum[A, B]

  final case class Success[A, B](value: B) extends Sum[A, B]

}

object Chapter5_5 {
  sealed trait LinkedList[A] {
    def map[B](fn: A => B): LinkedList[B] =
      this match {
        case Pair(hd, tl) => Pair(fn(hd), tl.map(fn))
        case End() => End[B]()
      }
  }

  final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

  final case class End[A]() extends LinkedList[A]

  /*sealed trait Maybe[A] {
  def flatMap[B](fn: A => Maybe[B]): Maybe[B] = {
  this match {
  case Full(v) => fn(v)
  case Empty() => Empty[B]()
  }
  }
  }
   *
  final case class Full[A](value: A) extends Maybe[A]
   *
  final case class Empty[A]() extends Maybe[A]*/

  // Ej 5.5.4.1
  val list: LinkedList[Int] = Pair(1, Pair(2, Pair(3, End())))

  list.map(_ * 2)
  list.map(_ + 1)
  list.map(_ / 3)

  // Ej 5.5.4.2
  sealed trait Maybe[A] {
    def map[B](fn: A => B): Maybe[B] = {
      this match {
        case Full(v) => Full(fn(v))
        case Empty() => Empty[B]()
      }
    }

    def mapFlatMap[B](fn: A => B): Maybe[B] =
      flatMap(v => Full(fn(v)))

    def flatMap[B](fn: A => Maybe[B]): Maybe[B] = {
      this match {
        case Full(v) => fn(v)
        case Empty() => Empty[B]()
      }
    }
  }

  final case class Full[A](value: A) extends Maybe[A]

  final case class Empty[A]() extends Maybe[A]

  // 5.5.4.3

  val listv2 = List(1, 2, 3)
  listv2.flatMap(v => List(v, -v))

  val listv3: List[Maybe[Int]] = List(Full(3), Full(2), Full(1))
  listv3.map(maybe => maybe.flatMap[Int] {
    x => {
      if (x % 2 == 0)
        Full(x)
      else
        Empty()
    }
  })

  // 5.5.4.4
  sealed trait Sum[A, B] {
    def fold[C](error: A => C, success: B => C): C =
      this match {
        case Failure(a) => error(a)
        case Success(b) => success(b)
      }

    def map[C](f: B => C): Sum[A, C] = {
      this match {
        case Failure(a) => Failure(a)
        case Success(b) => Success(f(b))
      }
    }

    def flatMap[C](f: B => Sum[A, C]): Sum[A, C] = {
      this match {
        case Failure(a) => Failure(a)
        case Success(b) => f(b)
      }
    }

  }

  final case class Failure[A, B](value: A) extends Sum[A, B]

  final case class Success[A, B](value: B) extends Sum[A, B]

}

object Chapter5_6 {

  // Ej 5.6.4.1
  // Ej 5.6.6.2
  sealed trait Sum[+A, +B] {
    def flatMap[AA >: A, C](f: B => Sum[AA, C]): Sum[AA, C] = {
      this match {
        case Failure(value) => Failure(value)
        case Success(value) => f(value)
      }
    }

    def map[C](f: B => C): Sum[A, C] =
      this match {
        case Failure(v) => Failure(v)
        case Success(v) => Success(f(v))
      }

    def fold[C](error: A => C, success: B => C): C =
      this match {
        case Failure(v) => error(v)
        case Success(v) => success(v)
      }
  }

  final case class Failure[A](value: A) extends Sum[A, Nothing]

  final case class Success[B](value: B) extends Sum[Nothing, B]

  sealed trait Expression {
    def eval: Sum[String, Double] =
      this match {
        case Addition(l, r) => lift2(l, r, (left, right) => Success(left + right))
        case Subtraction(l, r) => lift2(l, r, (left, right) => Success(left - right))
        case Division(l, r) => lift2(l, r, (left, right) =>
          if(right == 0)
            Failure("Division by zero")
          else
            Success(left / right)
        )
        case SquareRoot(v) =>
          v.eval flatMap { value =>
            if(value < 0)
              Failure("Square root of negative number")
            else
              Success(Math.sqrt(value))
          }
        case Number(v) => Success(v)
      }

    def lift2(l: Expression, r: Expression, f: (Double, Double) => Sum[String, Double]) =
      l.eval.flatMap { left =>
        r.eval.flatMap { right =>
          f(left, right)
        }
      }
  }
  final case class Addition(left: Expression, right: Expression) extends Expression
  final case class Subtraction(left: Expression, right: Expression) extends Expression
  final case class Division(left: Expression, right: Expression) extends Expression
  final case class SquareRoot(value: Expression) extends Expression
  final case class Number(value: Double) extends Expression

  assert(Addition(Number(1), Number(2)).eval == Success(3))
  assert(SquareRoot(Number(-1)).eval == Failure("Square root of negative number"))
  assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))
  assert(Division(Addition(Subtraction(Number(8), Number(6)), Number(2)), Number(2)).eval == Success(2.0))
}