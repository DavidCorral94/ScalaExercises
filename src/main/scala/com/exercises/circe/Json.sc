import io.circe.Json

val jsonString: Json = Json.fromString("scala exercises")
val jsonBoolean: Json = Json.fromBoolean(true)
val jsonDouble: Option[Json] = Json.fromDouble(1)
val fieldList =
  List(("key1", Json.fromString("value1")), ("key2", Json.fromInt(1)))
val jsonFromFields: Json = Json.fromFields(fieldList)

jsonFromFields.noSpaces
jsonFromFields.spaces2
jsonFromFields.spaces4

Json.fromFields(("key", Json.fromString("value")) :: Nil).noSpaces

"{\"name\":\"sample json\",\"data\":{\"done\":false}}" == (Json
  .fromFields(
    List(
      ("name", Json.fromString("sample json")),
      ("data", Json.fromFields(List(("done", Json.fromBoolean(false)))))
    )
  )
  .noSpaces)

"[{\"x\":1}]" == Json
  .arr(Json.fromFields(List(("x", Json.fromInt(1)))))
  .noSpaces

val jsonArray: Json = Json.fromValues(List(
  Json.fromFields(List(("field1", Json.fromInt(1)))),
  Json.fromFields(List(
    ("field1", Json.fromInt(200)),
    ("field2", Json.fromString("Having circe in Scala Exercises is awesome"))))))

def transformJson(jsonArray: Json): Json =
  jsonArray mapArray { oneJson: Vector[Json] =>
    oneJson.init
  }

transformJson(jsonArray).noSpaces