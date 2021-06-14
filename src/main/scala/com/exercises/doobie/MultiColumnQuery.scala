package com.exercises.doobie

import cats.effect.{ExitCode, IO, IOApp}
import doobie.ConnectionIO
import doobie.implicits._
import shapeless.record.Record
import shapeless.{HList, HNil}

object MultiColumnQuery extends IOApp {
  def transactorBlock[A](f: => ConnectionIO[A]): IO[A] = ???
  //transactor.use((createCountryTable *> insertCountries(countries) *> f).transact[IO])*

  /**
    * code    name                      population    gnp
    * "DEU"  "Germany"                    82164700    2133367.00
    * "ESP"  "Spain"                      39441700          null
    * "FRA"  "France",                    59225700    1424285.00
    * "GBR"  "United Kingdom"             59623400    1378330.00
    * "USA"  "United States of America"  278357000    8510700.00
    */

  override def run(args: List[String]): IO[ExitCode] = {

    val (name, population, gnp) =
      transactorBlock {
        sql"select name, population, gnp from country where code = 'ESP'"
          .query[(String, Int, Option[Double])]
          .unique
      }.unsafeRunSync()

    name == "Spain"
    population == "39441700"
    gnp == None

    type CountryHListType = String :: Int :: Option[Double] :: HNil

    val hlist: CountryHListType =
      transactorBlock {
        sql"select name, population, gnp from country where code = 'FRA'"
          .query[CountryHListType]
          .unique
      }.unsafeRunSync()

    hlist == HList("France", 59225700, 1424285.00)

    case class Country(code: String, name: String, population: Long, gnp: Option[Double])

    val country =
      transactorBlock {
        sql"select code, name, population, gnp from country where name = 'United Kingdom'"
          .query[Country]
          .unique
      }.unsafeRunSync()

    country.code == "GBR"

    case class Code(code: String)
    case class CountryInfo(name: String, pop: Int, gnp: Option[Double])

    val (codev2, countryv2) =
      transactorBlock {
        sql"select code, name, population, gnp from country where code = 'ESP'"
          .query[(Code, CountryInfo)]
          .unique
      }.unsafeRunSync()

    countryv2.name == "Spain"

    val notFoundCountry = CountryInfo("Not Found", 0, None)

    val countriesMap: Map[Code, CountryInfo] =
      transactorBlock {
        sql"select code, name, population, gnp from country"
          .query[(Code, CountryInfo)]
          .to[List]
      }.unsafeRunSync().toMap

    countriesMap.getOrElse(Code("DEU"), notFoundCountry).name == "Germany"
    countriesMap.get(Code("ITA")) == None

    IO.pure(ExitCode.Success)
  }
}
