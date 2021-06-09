package com.fd.modelling.day2.session4

import cats.data.EitherNec
import cats.implicits._

// Errors
sealed abstract class DomainError extends Product with Serializable
case class GenreNotSupported(givenValue: String) extends DomainError

// ADTs
sealed abstract class Genre extends Product with Serializable

case object Action extends Genre
case object Adventure extends Genre
case object Comedy extends Genre
case object Drama extends Genre
case object ScienceFiction extends Genre
case object Thriller extends Genre

object Genre {

  // Smart constructor in company object
  def fromString(genre: String): EitherNec[GenreNotSupported, Genre] =
    genre.toLowerCase() match {
      case "action"                     => Action.rightNec
      case "adventure"                  => Adventure.rightNec
      case "comedy"                     => Comedy.rightNec
      case "drama"                      => Drama.rightNec
      case "science fiction" | "sci-fi" => ScienceFiction.rightNec
      case "thriller"                   => Thriller.rightNec
      case _                            => GenreNotSupported(genre).leftNec
    }
}

object MainGenre {
  def main(args: Array[String]): Unit = {
    val genre = Genre.fromString("action")
    println(genre)
  }
}