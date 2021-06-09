package com.fd.modelling.day2.session4

import eu.timepit.refined._
import eu.timepit.refined.types.all._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._

import java.sql.Date
import java.time.LocalDate

object Data {
  /*
Validation rules
The person identifier should be a positive number
Their name shouldn't be empty
   */

  case class Person(id: PosLong, name: NonEmptyString, birthday: LocalDate)

}
object MainPerson {

  def main(args: Array[String]): Unit = {
    import Data.Person
    val p = Person(1L, "David", LocalDate.now())
    println(p.name)
  }
}
