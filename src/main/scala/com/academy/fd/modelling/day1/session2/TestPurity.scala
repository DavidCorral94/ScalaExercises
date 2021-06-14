package com.academy.fd.modelling.day1.session2

import org.scalatest.funsuite.AnyFunSuite

class TestSpec extends AnyFunSuite {

  test("Running pureCore should not log anything") {
    val testClass = new Purity()
    testClass.pureCore(List("a", "b", "c"))
    assert(testClass.fakeLog.toList == Nil)
  }

  test("Running mainProcedure should produce the correct results and side-effects") {
    val testClass = new Purity()
    val results   = testClass.mainProcedure
    val expectedResults =
      List("Ares", "Demeter", "Dionysus", "Zeus").map(s => s"Greek God/Goddess: $s")
    val expectedLogs = List("Got all names from across the network") ++
      List("Aphrodite", "Hermes").map(s => s"Filtered out name: $s") ++
      expectedResults.map(s => s"Publishing: $s")

    assert(results == expectedResults)
    assert(testClass.fakeLog.toList == expectedLogs)
  }
}
