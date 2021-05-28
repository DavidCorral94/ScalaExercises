import scala.collection.immutable.LazyList.cons

object ScalaExercises{
  val foodItem = "porridge"

  def goldilocks(expr: (String, String)) =
    expr match {
      case (`foodItem`, _) => "eating"
      case ("chair", "Mama") => "sitting"
      case ("bed", "Baby") => "sleeping"
      case _ => "what?"
    }

  goldilocks(("porridge", "Papa"))
  goldilocks(("chair", "Mama"))
  goldilocks(("porridge", "Cousin"))
  goldilocks(("beer", "Cousin"))

  val secondElement = List(1, 2, 3) match {
    case x :: xs => xs.head
    case _ => 0
  }

  case class Dog(name: String, breed: String)
  val d1 = Dog("Scooby", "Doberman")
  d1.toString

  val xValues = 1 to 4
  val yValues = 1 to 2
  val coordinates = for {
    x <- xValues
    y <- yValues
  } yield (x, y)

  val g: Int = 31
  g.toHexString

  val set = Set("Phoenix" -> 1, "Austin" -> 2)
  set.toMap

  def makeLazyList(v: Int): LazyList[Int] = cons(v, makeLazyList(v + 1))
  val a = makeLazyList(2)
  ((a drop 6) take 3).toList

  val stringList = List("Do", "Re", "Me", "Fa", "So", "La", "Te", "Do")
  stringList.reduceRight {
    _ + _
  }

  val intList = List(5, 4, 3, 2, 1)
  intList.reduceRight((x, y) => x - y)
  intList.reverse.reduceLeft((x, y) => y - x)
  intList.reverse.reduce((x, y) => y - x)

  classOf[String].getCanonicalName
  classOf[String].getClass
  classOf[String].getSimpleName

  //val a = 3.0
  val b = 3.00
  val c = 2.73
  val d = 3f
  val e = 3.22d
  val f = 93e-9
  val gh = 93e-9
  val h = 0.0
  val i = 9.23e-9d

  trait Fruit
  case class Orange() extends Fruit
  class MyContainer[+A](val a: A)(implicit manifest: scala.reflect.Manifest[A]) {
    def contents = manifest.runtimeClass.getSimpleName
  }

  //val fruitBasket: MyContainer[Fruit] = new MyContainer(new Orange())
  //fruitBasket.contents


  // Uncomment the following line
   val fruitBasket:MyContainer[Fruit] = new MyContainer[Orange](new Orange())
}