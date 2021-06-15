package com.exercises.doobie

import cats.effect.{ExitCode, IO, IOApp}
import doobie.ConnectionIO
import doobie.implicits._
import shapeless._
import com.exercises.doobie.DoobieUtils.CountryTable._
import com.exercises.doobie.Model._
import cats.implicits._
import com.exercises.doobie.Connecting.transactor

object MultiColumnQuery extends IOApp {

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

    val (name, population, gnp) =
      transactorBlock {
        sql"select name, population, gnp from country where code = 'ESP'"
          .query[(String, Int, Option[Double])]
          .unique
      }.unsafeRunSync()

    println((name, population, gnp))
    assert(name == "Spain")
    assert(population == 39441700)
    assert(gnp == None)

    type CountryHListType = String :: Int :: Option[Double] :: HNil

    val hlist: CountryHListType =
      transactorBlock {
        sql"select name, population, gnp from country where code = 'FRA'"
          .query[CountryHListType]
          .unique
      }.unsafeRunSync()

    println(hlist)
    // res = HList("France", 59225700, 1424285.00)
    assert(hlist.head == "France")

    val country =
      transactorBlock {
        sql"select code, name, population, gnp from country where name = 'United Kingdom'"
          .query[Country]
          .unique
      }.unsafeRunSync()

    assert(country.code == "GBR")

    val (codev2, countryv2) =
      transactorBlock {
        sql"select code, name, population, gnp from country where code = 'ESP'"
          .query[(Code, CountryInfo)]
          .unique
      }.unsafeRunSync()

    println((codev2, countryv2))
    assert(countryv2.name == "Spain")

    val notFoundCountry = CountryInfo("Not Found", 0, None)

    val countriesMap: Map[Code, CountryInfo] =
      transactorBlock {
        sql"select code, name, population, gnp from country"
          .query[(Code, CountryInfo)]
          .to[List]
      }.unsafeRunSync().toMap

    println(countriesMap)
    assert(
      countriesMap.getOrElse(Code("DEU"), notFoundCountry).name == "Germany"
    )
    assert(countriesMap.get(Code("ITA")) == None)

    IO.pure(ExitCode.Success)
  }
}
