object Chapter3 {
  println("Welcome to Chapter 3 of Essential Scala")

  /*
    class Person {
      val firstName = "Noel"
      val lastName = "Welsh"

      def name = firstName + " " + lastName
    }

    val noel = new Person

    noel.firstName

    object alien {
      def greet(p: Person) =
        "Greetings, " + p.firstName + " " + p.lastName
    }

    alien.greet(noel)
  */
  class Person(first: String, last: String) {
    val firstName = first
    val lastName = last

    def name = firstName + " " + lastName
  }

  val david = new Person("David", "Corral")

  object alien {
    def greet(p: Person) =
      "Greetings, " + p.firstName + " " + p.lastName
  }

  alien.greet(david)

  val test = new Person(last = "Last", first = "First")

  def greet(firstName: String = "Some", lastName: String = "Body") =
    "Greetings, " + firstName + " " + lastName + "!"

  greet(lastName = "Corral", firstName = "David")


  def badness = throw new Exception("Error")

  def otherbadness = null

  val bar = if (true) 123 else badness

  val baz = if (false) "it worked" else otherbadness

  // eJ 3.1.6.1

  /*
    class Cat(nameCat: String, colourCat: String, foodCat: String) {
      val name = nameCat
      val colour = colourCat
      val food = foodCat
    }
    */
  class Cat(val nameCat: String, val colourCat: String, val foodCat: String)

  val oswald = new Cat("Oswald", "Black", "Milk")
  val henderson = new Cat(colourCat = "Ginger", nameCat = "Henderson", foodCat = "Milk")
  val quentin = new Cat(foodCat = "Curry", colourCat = "Tabby and white", nameCat = "Quentin")

  object ChipShop {
    def willServe(cat: Cat): Boolean =
      if (cat.foodCat.equalsIgnoreCase("chips"))
        true
      else
        false
  }

  class Director(val firstName: String, val lastName: String, val yearOfBirth: Int) {
    def name = firstName + " " + lastName
  }

  class Film(val name: String, val yearOfRelease: Int, val imbdRating: Double, val director: Director) {
    def directorsAge: Double = yearOfRelease - director.yearOfBirth

    def isDirectedBy(d: Director): Boolean = d == director

    def copy(name: String = this.name, yearOfRelease: Int = this.yearOfRelease, imbdRating: Double = this.imbdRating, director: Director = this.director): Film
    = new Film(name, yearOfRelease, imbdRating, director)

  }

  val eastwood = new Director("Clint", "Eastwood", 1930)
  val mcTiernan = new Director("John", "McTiernan", 1951)
  val nolan = new Director("Christopher", "Nolan", 1970)
  val someBody = new Director("Just", "Some Body", 1990)

  val memento = new Film("Memento", 2000, 8.5, nolan)
  val darkKnight = new Film("Dark Knight", 2008, 9.0, nolan)
  val inception = new Film("Inception", 2010, 8.8, nolan)

  val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7, eastwood)
  val outlawJoseyWales = new Film("The Outlaw Josey Wales", 1976, 7.9, eastwood)
  val unforgiven = new Film("Unforgiven", 1992, 8.3, eastwood)
  val granTorino = new Film("Gran Torino", 2008, 8.2, eastwood)
  val invictus = new Film("Invictus", 2009, 7.4, eastwood)

  val predator = new Film("Predator", 1987, 7.9, mcTiernan)
  val dieHard = new Film("Die Hard", 1988, 8.3, mcTiernan)
  val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6, mcTiernan)
  val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8, mcTiernan)

  eastwood.yearOfBirth
  dieHard.director.name
  invictus.isDirectedBy(nolan)

  highPlainsDrifter.copy(name = "L'homme des hautes plaines")
  // res19: Film = Film(L'homme des hautes plaines,1973,7.7,Director(Clint,Eastwood,1930))

  thomasCrownAffair.copy(yearOfRelease = 1968,
    director = new Director("Norman", "Jewison", 1926))
  // res20: Film = Film(The Thomas Crown Affair,1968,6.8,Director(Norman,Jewison,1926))

  inception.copy().copy().copy()
  // res21: Film = Film(Inception,2010,8.8,Director(Christopher,Nolan,1970))

  class Counter(val count: Int = 0) {
    def inc: Counter = new Counter(count + 1)

    def inc(n: Int = 1): Counter = new Counter(count + n)

    def dec: Counter = new Counter(count - 1)

    def dec(n: Int = 1): Counter = new Counter(count - n)

    def adjust(adder: Adder): Counter = new Counter(adder.add(count))
  }

  new Counter(10).inc.dec.inc.inc.count
  new Counter(10).inc.inc(10).count

  class Adder(amount: Int) {
    def add(in: Int) = in + amount
  }

  new Counter(10).adjust(new Adder(120)).count
}

object Chapter3_2 {
  class Adder(amount: Int) {
    def apply(in: Int): Int = in + amount
  }

  val add3 = new Adder(3)
  // add3: Adder = Adder@4185f338

  add3.apply(2)
  // res0: Int = 5

  add3(4) // shorthand for add3.apply(4)
  // res1: Int = 7
}

object Chapter3_3 {
  class Timestamp(val seconds: Long)

  object Timestamp {
    def apply(hours: Int, minutes: Int, seconds: Int): Timestamp =
      new Timestamp(hours * 60 * 60 + minutes * 60 + seconds)
  }

  Timestamp(1, 1, 1).seconds

  // Ej 3.3.2.1

  class Person(val name: String, val surname: String)

  object Person {
    def apply(fullName: String): Person =
      new Person(fullName.split(" ")(0), fullName.split(" ")(1))
  }

  Person("David Corral").surname

  // Ej 3.3.2.2

  class Director(val firstName: String, val lastName: String, val yearOfBirth: Int) {
    def name = firstName + " " + lastName
  }

  object Director {
    def apply(firstName: String, lastName: String, yearOfBirth: Int): Director =
      new Director(firstName, lastName, yearOfBirth)

    def older(d1: Director, d2: Director): Director =
      if (d1.yearOfBirth > d2.yearOfBirth) d1 else d2
  }

  class Film(val name: String, val yearOfRelease: Int, val imbdRating: Double, val director: Director) {
    def directorsAge: Double = yearOfRelease - director.yearOfBirth

    def isDirectedBy(d: Director): Boolean = d == director

    def copy(name: String = this.name, yearOfRelease: Int = this.yearOfRelease, imbdRating: Double = this.imbdRating, director: Director = this.director): Film
    = new Film(name, yearOfRelease, imbdRating, director)

  }

  object Film {
    def apply(name: String, yearOfRelease: Int, imbdRating: Double, director: Director): Film =
      new Film(name, yearOfRelease, imbdRating, director)

    def highestRating(f1: Film, f2: Film): Double =
      if (f1.imbdRating > f2.imbdRating) f1.imbdRating else f2.imbdRating

    def oldestDirectorAtTheTime(f1: Film, f2: Film): Director = {
      val age1 = f1.yearOfRelease - f1.director.yearOfBirth
      val age2 = f2.yearOfRelease - f2.director.yearOfBirth
      if (age1 > age2) f1.director else f2.director
    }
  }
}

object Chapter3_4 {
  case class Person(firstName: String, lastName: String) {
    def name = firstName + " " + lastName
  }

  val dave = new Person("Dave", "Gurnell") // we have a class
  // dave: Person = Person(Dave,Gurnell)

  Person // and a companion object too
  // res0: Person.type = Person

  // Ej 3.4.5.1
  case class Cat(colour: String, food: String)

  val Osweld = Cat.apply("red", "tuna")
  val Henderson = Cat.apply("green", "fish")

  Osweld.toString

  // Ej 3.4.5.2

  /**
   * class Director(val firstName: String, val lastName: String, val yearOfBirth: Int) {
   * def name = firstName + " " + lastName
   * }
   */

  case class Director(firstName: String, lastName: String, yearOfBirth: Int)

  /** *
   * class Film(val name: String, val yearOfRelease: Int, val imbdRating: Double, val director: Director) {
   * def directorsAge: Double = yearOfRelease - director.yearOfBirth
   *
   * def isDirectedBy(d: Director): Boolean = d == director
   *
   * def copy(name: String = this.name, yearOfRelease: Int = this.yearOfRelease, imbdRating: Double = this.imbdRating, director: Director = this.director): Film
   * = new Film(name, yearOfRelease, imbdRating, director)
   *
   * }
   */

  case class Film(name: String, yearOfRelease: Int, imbdRating: Double, director: Director)

  /**
   * class Counter(val count: Int = 0) {
   * def inc: Counter = new Counter(count + 1)
   *
   * def inc(n: Int = 1): Counter = new Counter(count + n)
   *
   * def dec: Counter = new Counter(count - 1)
   *
   * def dec(n: Int = 1): Counter = new Counter(count - n)
   *
   * def adjust(adder: Adder): Counter = new Counter(adder.add(count))
   * }
   *
   * new Counter(10).inc.dec.inc.inc.count
   */

  // Ej 3.4.5.3
  case class Counter(count: Int = 0) {
    def inc = copy(count + 1)

    def inc(n: Int = 1) = copy(count + n)

    def dec = copy(count - 1)

    def dec(n: Int = 1) = copy(count - n)
  }

  Counter(10).inc.dec.inc.inc.count

  // Ej 3.4.5.4

  /**
   * class Person(val name: String, val surname: String)
   *
   * object Person {
   * def apply(fullName: String): Person =
   * new Person(fullName.split(" ")(0), fullName.split(" ")(1))
   * }
   */

  //case class Person(name: String, surname: String)

  object Person {
    def apply(name: String, surname: String): Person =
      new Person(name, surname)
  }
}

object Chapter3_5 {
  case class Person(firstName: String, lastName: String)

  object Stormtrooper {
    def inspect(person: Person): String =
      person match {
        case Person("Luke", "Skywalker") => "Stop, rebel scum!"
        case Person("Han", "Solo") => "Stop, rebel scum!"
        case Person(first, _) => s"Move along, $first"
      }
  }

  Stormtrooper.inspect(Person("Noel", "Welsh"))
  Stormtrooper.inspect(Person("Han", "Solo"))

  // Ej 3.5.3.1

  case class Cat(name: String, colour: String, food: String)

  object ChipShop {
    def willServe(cat: Cat): Boolean = {
      cat match {
        case Cat(_, _, "chips") => true
        case Cat(_, _, _) => false
      }
    }
  }

  ChipShop.willServe(Cat("Garfield", "Orange", "lasagna"))
  ChipShop.willServe(Cat("Kitty", "Black", "chips"))

  // Ej 3.5.3.2

  case class Director(firstName: String, lastName: String, yearOfBirth: Int)

  case class Film(name: String, yearOfRelease: Int, imbdRating: Double, director: Director)

  object Dad {
    def rate(film: Film): Double = {
      film match {
        case Film(_, _, _, Director("Clint", "Eastwood", _)) => 10.0
        case Film(_, _, _, Director("John", "McTiernan", _)) => 7.0
        case _ => 3.0
      }
    }
  }
}