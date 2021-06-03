object Chapter1_4 {
  import cats._
  import java.util.Date
  import cats.Show
  import cats.instances.int._ // for Show
  import cats.instances.string._ // for Show
  import cats.syntax.show._ // for show
  import cats.syntax.eq._ // for === and =!=

  implicit val dateShow: Show[Date] =
    Show.show(d => s"The date is ${d.getTime}")

  new Date().show

  final case class Cat(name: String, age: Int, color: String)

  object Cat {
    implicit val catShow: Show[Cat] = (t: Cat) => {
      s"The cat is ${t.name} ${t.age} ${t.age}"
    }

    // Exercise 1.5.5
    implicit val compare: Eq[Cat] = (c1: Cat, c2: Cat) =>
      c1.color.equalsIgnoreCase(c2.color)
  }

  val c1 = Option(Cat("David", 26, "Red"))
  val c2 = Option(Cat("David", 26, "Green"))

  c1.show
  c2.show

  Cat("a",1,"b") =!= Cat("a",2,"c")
  c1 === c2
  c1 === c1
}
