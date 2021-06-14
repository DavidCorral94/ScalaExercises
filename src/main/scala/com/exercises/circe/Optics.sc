import io.circe.Json
import io.circe.parser._

val json: Json = parse("""
{
  "order": {
    "customer": {
      "name": "Custy McCustomer",
      "contactDetails": {
        "address": "1 Fake Street, London, England",
        "phone": "0123-456-789"
      }
    },
    "items": [{
      "id": 123,
      "description": "banana",
      "quantity": 1
    }, {
      "id": 456,
      "description": "apple",
      "quantity": 2
    }],
    "total": 123.45
  }
}
""").getOrElse(Json.Null)

// Get phone number with cursors
val phoneNum: Option[String] = json.hcursor
  .downField("order")
  .downField("customer")
  .downField("contactDetails")
  .get[String]("phone")
  .toOption

// Alternative using optics
import io.circe.optics.JsonPath._
val _phone = root.order.customer.contactDetails.phone.string

val phone: Option[String] = _phone.getOption(json)

// Get items info with cursors
val items: Vector[Json] = json.hcursor
  .downField("order")
  .downField("items")
  .focus
  .flatMap(_.asArray)
  .getOrElse(Vector.empty)

val descriptions: Vector[String] =
  items.flatMap(_.hcursor.get[String]("description").toOption)

// Alternative using optics
val items = root.order.items.each.description.string.getAll(json)

val doubleQuantities: Json => Json =
  root.order.items.each.quantity.int.modify(_ * 2)

val modifiedJson = doubleQuantities(json)

val modifiedQuantities: List[Int] =
  root.order.items.each.quantity.int.getAll(modifiedJson)
modifiedQuantities == List(2, 4)

import io.circe.optics.JsonOptics._
import monocle.function.Plated

val recursiveModifiedJson = Plated.transform[Json] { j =>
  j.asNumber match {
    case Some(n) => Json.fromString(n.toString)
    case None => j
  }
}(json)

root.order.total.string.getOption(recursiveModifiedJson) == Some("123.45")

val doubleQuantities: Json => Json =
  root.order.items.each.quantity.int.modify(_ * 2) // Note the "itemss" typo

val modifiedJson = doubleQuantities(json)

// As there was a typo mistake, now, if we check the modifiedJson, it
// wont contains the updated fields because the path was wrong
val modifiedQuantitiesDynamic: List[Int] =
  root.order.items.each.quantity.int.getAll(modifiedJson)

modifiedQuantitiesDynamic != List(2, 4)