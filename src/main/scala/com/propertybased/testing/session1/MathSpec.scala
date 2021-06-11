package com.propertybased.testing.session1

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class MathSpec extends AnyFunSuite with Matchers {

  test("Math.abs should turn negative numbers positive") {
    val a = -5
    assert(Math.abs(a) > 0)
    assert(Math.abs(a) == 5)
  }

  test("Additional tests here") {
    val a = -5
    val b = Math.abs(a)
    assert(Math.abs(a) == Math.abs(b))
    assert(Math.abs(a) == b)
  }

}
