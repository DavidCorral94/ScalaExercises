object Chapter2 {
  // Ej 2.4.5.1
  object Cat1 {
    val name: String = "Oswald"
    val colour: String = "Black"
    val food: String = "Milk"
  }

  object Cat2 {
    val name: String = "Henderson"
    val colour: String = "Ginger"
    val food: String = "Chips"
  }

  object Cat3 {
    val name: String = "Quentin"
    val colour: String = "Tabby and white"
    val food: String = "Curry"
  }

  //Ej 2.4.5.2
  object calc {
    def square(n: Double): Double = n * n

    def cube(n: Double): Double = square(n) * n
  }

  calc.square(5.0)
  calc.cube(5.0)

  //Ej 2.4.5.3
  object calc2 {
    def square(n: Double): Double = n * n

    def square(n: Int): Int = n * n

    def cube(n: Double): Double = square(n) * n

    def cube(n: Int): Int = square(n) * n
  }

  calc2.square(5)
  calc2.cube(5)

  //Ej 2.4.5.4
  object argh {
    def a = {
      println("a")
      1
    }

    val b = {
      println("b")
      a + 2
    }

    def c = {
      println("c")
      a
      b + "c"
    }
  }

  argh.c + argh.b + argh.a

  //Ej 2.4.5.5
  object person {
    val firstName: String = "David"
    val secondName: String = "Corral"
  }

  object alien {
    def greet(p: person.type): String = "Hello " + p.firstName
  }

  alien.greet(person)

  // 2.5

  def square(in: Double): Double =
    in * in

  assert(square(2.0) == 4.0)
  assert(square(3.0) == 9.0)
  assert(square(-2.0) == 4.0)

  // 2.6

  if (1 < 2) "Yes" else "No"

  if (1 < 2) println("Yes") else println("No")

  {
    println("This is a side-effect")
    println("This is a side-effect as well")
    3
  }

  // Ej 2.6.4.1

  // String = "predator"
  if (1 > 2) "alien" else "predator"

  // Any = 2001 (I thought it would be Int = 2001, but Any is 'closer')
  if(1 > 2) "alien" else 2001

  // Any = () (I thought it would be Unit = (), but Any is 'closer')
  if(false) "hello"



}