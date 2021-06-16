import shapeless.Lazy

object Lazys {
  sealed trait List[+T]
  case class Cons[T](hd: T, tl: List[T]) extends List[T]
  sealed trait Nil extends List[Nothing]
  case object Nil extends Nil

  trait Show[T] {
    def apply(t: T): String
  }

  object Show {
    // Base case for Int
    implicit def showInt: Show[Int] =
      new Show[Int] {
        def apply(t: Int) = t.toString
      }

    // Base case for Nil
    implicit def showNil: Show[Nil] =
      new Show[Nil] {
        def apply(t: Nil) = "Nil"
      }

    // Case for Cons[T]: note (mutually) recursive implicit argument referencing Show[List[T]]
    implicit def showCons[T](implicit
        st: Lazy[Show[T]],
        sl: Lazy[Show[List[T]]]
    ): Show[Cons[T]] =
      new Show[Cons[T]] {
        def apply(t: Cons[T]) =
          s"Cons(${show(t.hd)(st.value)}, ${show(t.tl)(sl.value)})"
      }

    // Case for List[T]: note (mutually) recursive implicit argument referencing Show[Cons[T]]
    implicit def showList[T](implicit sc: Lazy[Show[Cons[T]]]): Show[List[T]] =
      new Show[List[T]] {
        def apply(t: List[T]) =
          t match {
            case n: Nil     => show(n)
            case c: Cons[T] => show(c)(sc.value)
          }
      }
  }

  def show[T](t: T)(implicit s: Show[T]) = s(t)

  val l: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))
  show(l) == "Cons(1, Cons(2, Cons(3, Nil)))"
}
