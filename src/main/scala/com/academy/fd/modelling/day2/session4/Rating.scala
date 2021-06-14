package com.academy.fd.modelling.day2.session4

case class Rating private (description: String) extends AnyVal

object Rating {
  private def apply(description: String): Rating = new Rating(description)

  val general = new Rating(
    "All ages admitted. Nothing that would offend parents for viewing by children."
  )
  val pg = new Rating(
    "Some material may not be suitable for children. Parents urged to give \"parental guidance\"."
  )
  val pg13 = new Rating(
    "Some material may be inappropriate for children under 13. Parents are urged to be cautious."
  )
  val restricted = new Rating(
    "Under 17 requires accompanying parent or adult guardian. Contains some adult material."
  )
  val nc17 = new Rating(
    "No One 17 and Under Admitted. Clearly adult. Young children will not be admitted to watch the film."
  )
}

object MainRating {
  def main(args: Array[String]): Unit = {
    val rating = Rating.nc17
    println(rating.description)
  }
}
