package com.propertybased.testing.session1

import org.scalacheck.Prop.forAll

import org.scalatest.funsuite.AnyFunSuite

class StringSpec extends AnyFunSuite {

  test("Reversing a string twice results in the same string") {
    forAll { s: String => s.reverse.reverse == s }
  }

  test("Reversed string should remain the same length") {
    forAll { s: String => s.reverse.length == s.length }
  }
}
