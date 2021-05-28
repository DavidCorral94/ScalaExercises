object StrictinessAndLaziness {

  case class Cons[A](head: A, tail: Stream[A])

  def cons[A](hd: => A, tl: => List[A]) = {
    hd :: tl
  }

//  def empty[A]: Stream[A] = Empty

  val startingPoint = Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList

  val step1 = cons(1, List(2, 3, 4).map(_ + 10)).filter(_ % 2 == 0).toList

  val step2 = List(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList

  val step3 = cons(12, List(3, 4).map(_ + 10)).filter(_ % 2 == 0).toList

  // Apply filter to the second element. Produce the first element of the result:
  val step4 = 12 :: List(3, 4).map(_ + 10).filter(_ % 2 == 0).toList
  val step5 = 12 :: cons(3, List(4).map(_ + 10)).filter(_ % 2 == 0).toList
  val step6 = 12 :: List(4).map(_ + 10).filter(_ % 2 == 0).toList
  val step7 = 12 :: cons(14, List[Int]().map(_ + 10)).filter(_ % 2 == 0).toList

  //val ones: Stream[Int] = Stream.cons(1, ones)


  List(1,2,3).scanRight(0)(_+_)

}
