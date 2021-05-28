import scala.reflect.runtime.universe.Try
import scala.util.Success

object HandlingError {

  case class Employee(name: String, department: String, manager: Option[String])

  def lookupByName(name: String): Option[Employee] =
    name match {
      case "Joe"   => Some(Employee("Joe", "Finances", Some("Julie")))
      case "Mary"  => Some(Employee("Mary", "IT", None))
      case "Izumi" => Some(Employee("Izumi", "IT", Some("Mary")))
      case _       => None
    }

  def getDepartment: (Option[Employee]) => Option[String] =
    _.map(_.department)

  getDepartment(lookupByName("Joe")) == Some("Finances")
  getDepartment(lookupByName("Mary")) == Some("IT")
  getDepartment(lookupByName("Foo")) == None

  def getManager: (Option[Employee]) => Option[String] =
    _.flatMap(_.manager)

  getManager(lookupByName("Joe")) == Some("Julie")
  getManager(lookupByName("Mary")) == None
  getManager(lookupByName("Foo")) == None
  getManager(lookupByName("Foo")).orElse(Some("LOL")) == Some("LOL")

  lookupByName("Joe").filter(_.department != "IT") == Some(
    Employee("Joe", "Finances", Some("Julie"))
  )
  lookupByName("Mary").filter(_.department != "IT") == None
  lookupByName("Foo").filter(_.department != "IT") == None

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t =>
        h flatMap (hh =>
          sequence(t) map {
            println(s"Mapeando $hh con $t")
            hh :: _
          }
        )
    }

  sequence(List(Some(1), Some(2), Some(3)))

  val list1 = List("1", "2", "3")
  val list2 = List("I", "II", "III", "IV")

  def lookupByNameViaEither(name: String): Either[String, Employee] =
    name match {
      case "Joe"   => Right(Employee("Joe", "Finances", Some("Julie")))
      case "Mary"  => Right(Employee("Mary", "IT", None))
      case "Izumi" => Right(Employee("Izumi", "IT", Some("Mary")))
      case _       => Left("Employee not found")
    }

  def getDepartmentEither
      : (Either[String, Employee]) => Either[String, String] =
    _.map(_.department)

  def getManager(employee: Either[String, Employee]): Either[String, String] =
    employee.flatMap(e => {
      println(s" Empleado => $e")
      e.manager match {
        case Some(e) => Right(e)
        case _       => Left("Manager not found")
      }
    })

  getManager(lookupByNameViaEither("Joe"))
  getManager(lookupByNameViaEither("Mary"))
  getManager(
    lookupByNameViaEither("Foo")
  ) //  Why no Left("Manager not found") ?

  getManager(lookupByNameViaEither("Joe")).orElse(Right("Mr. CEO"))
  getManager(lookupByNameViaEither("Mary")).orElse(Right("Mr. CEO"))
  getManager(lookupByNameViaEither("Foo")).orElse(Right("Mr. CEO"))

  def map2(a: Either[String, Employee])(b: Either[String, Employee])(f: (Employee, Employee) => Boolean) =
    for {
      a1 <- a
      b1 <- b
    } yield f(a1, b1)

  def employeesShareDepartment(employeeA: Employee, employeeB: Employee) =
    employeeA.department == employeeB.department


  map2(lookupByNameViaEither("Joe"))(lookupByNameViaEither("Mary"))(
    employeesShareDepartment)
}
