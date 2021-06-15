package com.exercises.doobie

import cats.effect.{ExitCode, IO, IOApp}
import com.exercises.doobie.Connecting.transactor
import doobie.ConnectionIO
import doobie.implicits._
import com.exercises.doobie.DoobieUtils.CountryTable._
import com.exercises.doobie.Model._
import cats.implicits._

object SelectingData extends IOApp {

  /**
    * code    name                      population    gnp
    * "DEU"  "Germany"                    82164700    2133367.00
    * "ESP"  "Spain"                      39441700          null
    * "FRA"  "France",                    59225700    1424285.00
    * "GBR"  "United Kingdom"             59623400    1378330.00
    * "USA"  "United States of America"  278357000    8510700.00
    */

  def transactorBlock[A](f: => ConnectionIO[A]): IO[A] =
    transactor.use(
      (createCountryTable *> dropCountries *> insertCountries(countries) *> f)
        .transact[IO]
    )

  override def run(args: List[String]): IO[ExitCode] = {
    val countryName =
      transactorBlock(
        sql"select name from COUNTRY where code = 'ESP'".query[String].unique
      ).unsafeRunSync()

    println(countryName)
    println(countryName == "Spain")

    val maybeCountryName =
      transactorBlock(
        sql"select name from country where code = 'ITA'".query[String].option
      ).unsafeRunSync()

    println(maybeCountryName)
    println(maybeCountryName == None)

    val countryNames =
      transactorBlock {
        sql"select name from country order by name".query[String].to[List]
      }.unsafeRunSync()

    println(countryNames)
    println(
      countryNames == List(
        "France",
        "Germany",
        "Spain",
        "United Kingdom",
        "United States of America"
      )
    )

    val firstThreecountryNames =
      transactorBlock {
        sql"select name from country order by name"
          .query[String]
          .stream
          .take(3)
          .compile
          .toList
      }.unsafeRunSync()

    println(firstThreecountryNames)
    println(firstThreecountryNames == List("France", "Germany", "Spain"))

    IO.pure(ExitCode.Success)
  }
}
