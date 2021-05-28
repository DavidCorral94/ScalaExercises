
object Chapter6_1 {

  // Sequences

  val sequence = Seq(1, 2, 3, 4)
  sequence.headOption
  sequence(2)

  sequence.find(e => e % 2 == 0 && e > 2)
  val seqFiltered = sequence.filter(_ > 2)

  sequence.sortWith(_ > _)

  sequence :+ 5

  0 +: sequence

  0 +: sequence :+ 5

  sequence ++ Seq(5, 6, 7)

  // Lists

  val list = 1 :: 2 :: 3 :: Nil

  List(1, 2, 3) ::: List(4, 5, 6)

  (List(1, 2, 3) ::: List(4, 5, 6)) :: Nil

  // Importing

  import scala.collection.immutable._

  val v = Vector(1, 2, 3)
  v.tail

  val q = Queue(1, 2, 3)
  q.head

  // Ej 6.1.9.1
  // There is a synonym of length defined on Seq—what is it called?
  // size == length
  sequence.size

  // There are two methods for retrieving the first item in a List – what are they called and how do they differ?
  sequence.head // returns a value or a Except
  sequence.headOption // returns an option with (or not) a value
  Seq().headOption

  // What method can be used to display the elements of the sequence as a string?
  sequence.mkString(" |*| ")

  // What method of Option can be used to determine whether the option contains a value?
  sequence.headOption.isDefined
  sequence.headOption.isEmpty

  // Ej 6.1.9.2
  val animals = Seq("cat", "dog", "penguin")
  "mouse" +: animals :+ "tyrannosaurus"
  2 +: animals

  // Ej 6.1.9.3

  case class Film(
                   name: String,
                   yearOfRelease: Int,
                   imdbRating: Double)

  case class Director(
                       firstName: String,
                       lastName: String,
                       yearOfBirth: Int,
                       films: Seq[Film])

  val memento = new Film("Memento", 2000, 8.5)
  val darkKnight = new Film("Dark Knight", 2008, 9.0)
  val inception = new Film("Inception", 2010, 8.8)

  val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7)
  val outlawJoseyWales = new Film("The Outlaw Josey Wales", 1976, 7.9)
  val unforgiven = new Film("Unforgiven", 1992, 8.3)
  val granTorino = new Film("Gran Torino", 2008, 8.2)
  val invictus = new Film("Invictus", 2009, 7.4)

  val predator = new Film("Predator", 1987, 7.9)
  val dieHard = new Film("Die Hard", 1988, 8.3)
  val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6)
  val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8)

  val eastwood = new Director("Clint", "Eastwood", 1930,
    Seq(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino, invictus))

  val mcTiernan = new Director("John", "McTiernan", 1951,
    Seq(predator, dieHard, huntForRedOctober, thomasCrownAffair))

  val nolan = new Director("Christopher", "Nolan", 1970,
    Seq(memento, darkKnight, inception))

  val someGuy = new Director("Just", "Some Guy", 1990,
    Seq())

  val directors = Seq(eastwood, mcTiernan, nolan, someGuy)

  def directorsWithBackCatalogOfSize(numberOfFilms: Int): Seq[Director] =
    directors.filter(_.films.size > numberOfFilms)

  def directorBornBefore(year: Int): Option[Director] =
    directors.find(_.yearOfBirth < year)

  def directorsBornBeforeAndWithNumFilms(year: Int, numberOfFilms: Int): Seq[Director] =
    directors.filter(d => d.films.size > numberOfFilms && d.yearOfBirth < year)

  /**
   * def directorBornBeforeWithBackCatalogOfSize(year: Int, numberOfFilms: Int): Seq[Director] = {
   * val byAge   = directors.filter(_.yearOfBirth < year)
   * val byFilms = directors.filter(_.films.length > numberOfFilms)
   * byAge.filter(byFilms.contains)
   * }
   */

  def directorsSortedByAge(ascending: Boolean = true) =
    directors.sortWith { (a, b) =>
      if (ascending) {
        a.yearOfBirth < b.yearOfBirth
      } else {
        a.yearOfBirth > b.yearOfBirth
      }
    }
}

object Chapter6_2 {
  val sequence = Seq(1, 2, 3, 4)
  sequence.map(_ * 2)

  "dog".permutations.toList
  //Seq("a", "wet", "dog").map(_.permutations.toList)
  //Seq("a", "wet", "dog").flatMap(_.permutations.toList)

  import scala.collection.immutable.Vector

  Vector(1, 2, 3).flatMap(num => Seq(num, num * 10))

  // Folds
  Seq(1, 2, 3).foldLeft(0)(_ + _)
  // (((0 + 1) + 2) + 3)
  Seq(1, 2, 3).foldLeft(0)(_ + _)
  // (1 + (2 + (3 + 0)))
  Seq(1, 2, 3).foldLeft(1)(_ * _)
  // (1 * (2 * (3 * 1)))

  // For eachs
  List(1, 2, 3).foreach(num => println("And a " + num + "..."))


  // Ej 6.2.7.1

  case class Film(
                   name: String,
                   yearOfRelease: Int,
                   imdbRating: Double)

  case class Director(
                       firstName: String,
                       lastName: String,
                       yearOfBirth: Int,
                       films: Seq[Film])

  val memento = new Film("Memento", 2000, 8.5)
  val darkKnight = new Film("Dark Knight", 2008, 9.0)
  val inception = new Film("Inception", 2010, 8.8)

  val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7)
  val outlawJoseyWales = new Film("The Outlaw Josey Wales", 1976, 7.9)
  val unforgiven = new Film("Unforgiven", 1992, 8.3)
  val granTorino = new Film("Gran Torino", 2008, 8.2)
  val invictus = new Film("Invictus", 2009, 7.4)

  val predator = new Film("Predator", 1987, 7.9)
  val dieHard = new Film("Die Hard", 1988, 8.3)
  val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6)
  val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8)

  val eastwood = new Director("Clint", "Eastwood", 1930,
    Seq(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino, invictus))

  val mcTiernan = new Director("John", "McTiernan", 1951,
    Seq(predator, dieHard, huntForRedOctober, thomasCrownAffair))

  val nolan = new Director("Christopher", "Nolan", 1970,
    Seq(memento, darkKnight, inception))

  val someGuy = new Director("Just", "Some Guy", 1990,
    Seq())

  val directors = Seq(eastwood, mcTiernan, nolan, someGuy)

  mcTiernan.films.sortWith { (a, b) =>
    a.yearOfRelease < b.yearOfRelease
  }.headOption
  val nolanFilms = nolan.films.map(_.name)

  val allMovies = directors.flatMap(d => d.films.map(_.name))

  mcTiernan.films.map(_.yearOfRelease).min

  /*
    val earliestMcTiernan =
      mcTiernan.films.min{
        (a,b) =>
          a.yearOfRelease < b.yearOfRelease
      }

  */
  /*
    directors.flatMap(d => d.films.sortWith {
      (a, b) =>
        a.imdbRating > b.imdbRating
    })*/

  //val films = directors.flatMap(_.films)

  //films.foldRight(0.0)((film, sum) => sum + film.imdbRating) / films.size

  directors.foreach {
    d =>
      d.films.foreach {
        f => println("Tonight only! " + f.name + " by " + d.firstName)
      }
  }

  directors.flatMap(d => d.films).sortWith {
    (a, b) =>
      a.yearOfRelease < b.yearOfRelease
  }.headOption

  // Ej 6.2.7.2
  def min(seq: Seq[Int]): Option[Int] = {
    seq.sortWith((a, b) => a < b).headOption
  }

  def smallest(seq: Seq[Int]): Int =
    seq.foldLeft(Int.MaxValue)(math.min)

  min(Seq(12, 43, 52, 4, 12))

  smallest(Seq())

  val s = Seq(1, 1, 2, 4, 3, 4)

  def insert(seq: Seq[Int], elt: Int): Seq[Int] = {
    if (seq.contains(elt))
      seq
    else
      elt +: seq
  }

  def unique(seq: Seq[Int]): Seq[Int] = {
    seq.foldLeft(Seq.empty[Int]) {
      insert
    }
  }

  unique(Seq(1, 1, 2, 4, 3, 4))

  def reverse(seq: Seq[Int]): Seq[Int] = {
    seq.foldLeft(Seq.empty[Int]) { (seq, elt) => elt +: seq }
  }

  reverse(Seq(1, 1, 2, 4, 3, 4))

  def map[A, B](seq: Seq[A], f: A => B): Seq[B] = {
    seq.foldRight(Seq.empty[B]) { (elt, seq) => f(elt) +: seq }
  }


}

object Chapter6_3 {
  Seq(1, 2, 3).map(_ * 2)

  for {
    x <- Seq(1, 2, 3)
  } yield x * 2

  val data = Seq(Seq(1), Seq(2, 3), Seq(4, 5, 6))
  data.flatMap(_.map(_ * 2))

  for {
    subseq <- data
    element <- subseq
  } yield element * 2

  // Ej 6.3.1

  case class Film(
                   name: String,
                   yearOfRelease: Int,
                   imdbRating: Double)

  case class Director(
                       firstName: String,
                       lastName: String,
                       yearOfBirth: Int,
                       films: Seq[Film])

  val memento = new Film("Memento", 2000, 8.5)
  val darkKnight = new Film("Dark Knight", 2008, 9.0)
  val inception = new Film("Inception", 2010, 8.8)

  val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7)
  val outlawJoseyWales = new Film("The Outlaw Josey Wales", 1976, 7.9)
  val unforgiven = new Film("Unforgiven", 1992, 8.3)
  val granTorino = new Film("Gran Torino", 2008, 8.2)
  val invictus = new Film("Invictus", 2009, 7.4)

  val predator = new Film("Predator", 1987, 7.9)
  val dieHard = new Film("Die Hard", 1988, 8.3)
  val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6)
  val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8)

  val eastwood = new Director("Clint", "Eastwood", 1930,
    Seq(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino, invictus))

  val mcTiernan = new Director("John", "McTiernan", 1951,
    Seq(predator, dieHard, huntForRedOctober, thomasCrownAffair))

  val nolan = new Director("Christopher", "Nolan", 1970,
    Seq(memento, darkKnight, inception))

  val someGuy = new Director("Just", "Some Guy", 1990,
    Seq())

  val directors = Seq(eastwood, mcTiernan, nolan, someGuy)

  for {
    f <- nolan.films
  } yield f.name

  for {
    d <- directors
    f <- d.films
  } yield f.name

  for {
    d <- directors
    f <- d.films.sortWith((a, b) => a.imdbRating > b.imdbRating)
  } yield f

  for {
    d <- directors
    f <- d.films
  } yield {
    println("Tonight only! " + f.name + " by " + d.firstName)
  }
}

object Chapter6_5 {
  // Ej 6.5.1.1

  def addOptions(optionA: Option[Int], optionB: Option[Int]): Option[Int] = {
    for {
      a <- optionA
      b <- optionB
    } yield a + b
  }
  /*
    def addOptions(optionA: Option[Int], optionB: Option[Int], optionC: Option[Int]): Option[Int] = {
      for {
        a <- optionA
        b <- optionB
        c <- optionC
      } yield a + b + c
    }

    def addOptionsMapped(optionA: Option[Int], optionB: Option[Int]): Option[Int] = {
      optionA.flatMap(a => optionB.map(b => a + b))
    }

    def addOptionsMapped(optionA: Option[Int], optionB: Option[Int], optionC: Option[Int]): Option[Int] = {
      optionA.flatMap(a => optionB.flatMap(b => optionC.map(c => a + b + c)))
    }

    addOptions(Some(4), Some(1))
    addOptions(Some(4), Some(1), Some(1))
    addOptions(Some(4), None)

    addOptionsMapped(Some(4), Some(1))
    addOptionsMapped(Some(4), Some(1), Some(1))
    addOptionsMapped(Some(4), None)*/

  // Ej 6.5.1.3

  def divide(numerator: Int, denominator: Int) =
    if (denominator == 0) None else Some(numerator / denominator)

  def divideOptions(numerator: Option[Int], denominator: Option[Int]) =
    for {
      a <- numerator
      b <- denominator
      c <- divide(a, b)
    } yield c

  divideOptions(Some(1), None)

  // Ej 6.5.1.4

  def calculator(operand1: String, operator: String, operand2: String): Unit = {
    /*val result = for {
      a <- readInt(operand1)
      b <- readInt(operand2)
      ans <- operator match {
        case "+" => Some(a + b)
        case "-" => Some(a - b)
        case "*" => Some(a * b)
        case "/" => divide(a, b)
        case _ => None
      }
    } yield ans

    result match {
      case Some(n) => println("Resultado es " + n)
      case None => print("Error")
    }*/

    def calcInternal(a: Int, b: Int) =
      operator match {
        case "+" => Some(a + b)
        case "-" => Some(a - b)
        case "*" => Some(a * b)
        case "/" => divide(a, b)
        case _ => None
      }

    val result = readInt(operand1)
      .flatMap(opA => readInt(operand2)
        .flatMap(opB => calcInternal(opA, opB)))

    result match {
      case Some(number) => println(s"The answer is $number!")
      case None => println(s"Error calculating $operand1 $operator $operand2")
    }
  }

  def readInt(str: String): Option[Int] =
    if (str matches "-?\\d+") Some(str.toInt) else None

  calculator("1", "+", "2")
  calculator("1", "/", "2")

}

object Chapter6_6 {
  // Ej 6.6.2.1

  import scala.util.Try

  val opt1 = Some(1)
  val opt2 = Some(2)
  val opt3 = Some(3)

  for {
    a <- opt1
    b <- opt2
    c <- opt3
  } yield a + b + c

  val seq1 = Seq(1)
  val seq2 = Seq(2)
  val seq3 = Seq(3)

  for {
    a <- seq1
    b <- seq2
    c <- seq3
  } yield a + b + c

  val try1 = Try(1)
  val try2 = Try(2)
  val try3 = Try(3)

  for {
    a <- try1
    b <- try2
    c <- try3
  } yield a + b + c

}

object Chapter6_7 {

  for (x <- Seq(-2, -1, 0, 1, 2) if x > 0) yield x

  Seq(1, 2, 3).zip(Seq(4, 5, 6))

  for (x <- Seq(1, 2, 3).zip(Seq(4, 5, 6))) yield {
    val (a, b) = x; a + b
  }

  // or

  for ((a, b) <- Seq(1, 2, 3).zip(Seq(4, 5, 6))) yield a + b


  for (x <- Seq(1, 2, 3).zipWithIndex) yield x

  for ((a, b) <- Seq(1, 2, 3).zip(Seq(4, 5, 6))) yield a + b

  for {
    x <- Seq(1, 2, 3)
    square = x * x
    y <- Seq(4, 5, 6)
  } yield square * y

}

object Chapter6_8 {

  val example = Map("a" -> 1, "b" -> 2, "c" -> 3)

  example.getOrElse("d", -1)

  example.contains("a")

  example.size

  //example.+("c" -> 10, "d" -> 11, "e" -> 12)

  example.-("b", "c")


  val example2 = scala.collection.mutable.Map("x" -> 10, "y" -> 11, "z" -> 12)

  example2 += ("x" -> 20)
  /*
    example2.subtractAll(Seq("y", "z"))

    //scala.collection.immutable.ListMap("a" -> 1) + ("b" -> 2) + ("c" -> 3) + ("d" -> 4) + ("e" -> 5)
    example.map(pair => pair._1 -> pair._2 * 2)
    example.map(pair => pair._1 + " = " + pair._2)

    example.flatMap {
      case (str, num) =>
        (1 to 3).map(x => (str + x) -> (num * x))
    }

    // or

    for{
      (str, num) <- example
      x         <- 1 to 3
    } yield (str + x) -> (num * x)

    for{
      (str, num) <- example
      x         <- 1 to 3
    } yield (x + str) + "=" + (x * num)

  */

  // Ej 6.8.3.1
  val people = Set(
    "Alice",
    "Bob",
    "Charlie",
    "Derek",
    "Edith",
    "Fred")

  val ages = Map(
    "Alice" -> 20,
    "Bob" -> 30,
    "Charlie" -> 50,
    "Derek" -> 40,
    "Edith" -> 10,
    "Fred" -> 60)

  val favoriteColors = Map(
    "Bob" -> "green",
    "Derek" -> "magenta",
    "Fred" -> "yellow")

  val favoriteLolcats = Map(
    "Alice" -> "Long Cat",
    "Charlie" -> "Ceiling Cat",
    "Edith" -> "Cloud Cat")

  def favoriteColor(name: String): String =
    favoriteColors.getOrElse(name, "beige")

  def printColors = {
    favoriteColors.foreach(c => println(c._2))
  }

  def lookup[B](name: String, map: Map[String, B]): Option[B] =
    map.get(name)

  lookup("Alice", favoriteLolcats)

  ages.map(f => f._1 -> f._2)


  val oldest: Option[String] =
    people.foldLeft(Option.empty[String]) { (older, person) =>
      if (ages.getOrElse(person, 0) > older.flatMap(ages.get).getOrElse(0)) {
        Some(person)
      } else {
        older
      }
    }


  val oldestOwn: Option[String] =
    people.foldLeft(Option.empty[String]) { (older, person) => {
      //older.flatMap(p => ages.get(p))
      // In older you are storing a Some(Person)
      // With flatmap we enter into that person
      // And with ages.get (simplified syntax) we get
      // the age of that person
      // which is the thing used in the comparison
      if (ages.getOrElse(person, 0) > older.flatMap(ages.get).getOrElse(0)) {
        Some(person)
      } else {
        older
      }
    }
    }

  val favorite: Option[String] =
    for {
      oldest <- oldest
      color <- favoriteColors.get(oldest)
    } yield color

}

object Chapter6_9 {
  1 until 10

  10 until 1 by -1

  for (i <- 99 until 0 by -1) println(i + " bottles of beer on the wall!")

  (1 until 10) ++ (20 until 30)

}

object Chapter6_10 {
  val subjects = List("Noel", "The cat", "The dog")
  val verbs = List("wrote", "chased", "slept on")
  val objects = List("the book", "the ball", "the bed")

  def allSentences: List[(String, String, String)] =
    for {
      subject <- subjects
      verb <- verbs
      obj <- objects
    } yield (subject, verb, obj)


  def verbsFor(subject: String): List[String] =
    subject match {
      case "Noel" => List("wrote", "chased", "slept on")
      case "The cat" => List("meowed at", "chased", "slept on")
      case "The dog" => List("barked at", "chased", "slept on")
    }

  def objectsFor(verb: String): List[String] =
    verb match {
      case "wrote" => List("the book", "the letter", "the code")
      case "chased" => List("the ball", "the dog", "the cat")
      case "slept on" => List("the bed", "the mat", "the train")
      case "meowed at" => List("Noel", "the door", "the food cupboard")
      case "barked at" => List("the postman", "the car", "the cat")
    }

  def allSentencesConditional: List[(String, String, String)] =
    for {
      subject <- subjects
      verb <- verbsFor(subject)
      obj <- objectsFor(verb)
    } yield (subject, verb, obj)

  final case class Distribution[A](events: List[(A, Double)]) {
    def map[B](f: A => B): Distribution[B] = {
      Distribution(events map {
        case (a, p) => f(a) -> p
      })
    }

    def flatMap[B](f: A => Distribution[B]): Distribution[B] = {
      Distribution(events flatMap { case (a, p1) =>
        f(a).events map { case (b, p2) => b -> (p1 * p2) }
      }).compact.normalize
    }

    def normalize: Distribution[A] = {
      val totalWeight = (events map { case (a, p) => p }).sum
      Distribution(events map { case (a, p) => a -> (p / totalWeight) })
    }

    def compact: Distribution[A] = {
      val distinct = (events map { case (a, p) => a }).distinct

      def prob(a: A): Double =
        (events filter { case (x, p) => x == a } map { case (a, p) => p }).sum

      Distribution(distinct map { a => a -> prob(a) })
    }

  }

  object Distribution {
    def uniform[A](atoms: List[A]): Distribution[A] = {
      val p = 1.0 / atoms.length
      Distribution(atoms.map(a => a -> p))
    }

    def discrete[A](events: List[(A, Double)]): Distribution[A] =
      Distribution(events).compact.normalize
  }

  sealed trait Coin

  case object Heads extends Coin

  case object Tails extends Coin

  val fairCoin: Distribution[Coin] = Distribution.uniform(List(Heads, Tails))

  val threeFlips =
    for {
      c1 <- fairCoin
    } yield (c1)
}