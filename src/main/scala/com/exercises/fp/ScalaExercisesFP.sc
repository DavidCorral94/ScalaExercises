object ScalaExercisesFP {

  /*sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]
*/
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case x :: xs if f(x) => dropWhile(xs, f)
      case _ => l
    }

  dropWhile(List(1, 2, 3), (x: Int) => {x < 2})
  dropWhile(List(1, 2, 3), (x: Int) => {x > 2})
  
}
