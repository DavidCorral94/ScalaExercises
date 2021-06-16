import shapeless.{HMap, HNil}

object HeterogeneousMaps {
  class BiMapIS[K, V]
  implicit val intToString = new BiMapIS[Int, String]
  implicit val stringToInt = new BiMapIS[String, Int]
  //implicit val intToInt = new BiMapIS[Int, Int]

  val hm = HMap[BiMapIS](23 -> "foo", "bar" -> 13, 100 -> "joe")
  //val hm2 = HMap[BiMapIS](23 -> "foo", 23 -> 13)

  hm.get(23) == Some("foo")
  hm.get("bar") == Some(13)

  import hm._
  val l = 23 :: "bar" :: 100 :: HNil
  val m = l map hm


}
