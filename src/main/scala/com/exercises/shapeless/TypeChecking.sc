import org.scalatest.Assertions.assertTypeError

import scala.util.Try

object TypeChecking {
  import shapeless.test.illTyped
  illTyped { """1+1 : Boolean""" }
  //illTyped { """1+1 : Int""" }


  val matchedTypes = Try(assertTypeError("illTyped { \"val a: Int = 1\" }")).isSuccess
  matchedTypes == true

  val mismatchedTypes = Try(assertTypeError("illTyped { \"val a: String = 1\" }")).isSuccess
  mismatchedTypes == false
}