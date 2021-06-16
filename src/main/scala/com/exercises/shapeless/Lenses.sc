object Lenses {
  import shapeless._
  import syntax._

  case class Address(street: String, city: String, postcode: String)
  case class Person(name: String, age: Int, address: Address)

  // Some lenses over Person/Address ...
  val nameLens = lens[Person] >> Symbol("name")
  val ageLens = lens[Person] >> Symbol("age")
  val addressLens = lens[Person] >> Symbol("address")
  val streetLens = lens[Person] >> Symbol("address") >> Symbol("street")
  val cityLens = lens[Person] >> Symbol("address") >> Symbol("city")
  val postcodeLens = lens[Person] >> Symbol("address") >> Symbol("postcode")

  val person =
    Person("Joe Grey", 37, Address("Southover Street", "Brighton", "BN2 9UA"))

  ageLens.get(person) == 37

  val updatedPerson = ageLens.set(person)(38)
  updatedPerson.age == 38

  val updatedPerson = ageLens.modify(person)(_ + 1)
  updatedPerson.age == 38

  streetLens.get(person) == "Southover Street"

  val updatedPerson = streetLens.set(person)("Montpelier Road")
  updatedPerson.address.street == "Montpelier Road"
}
