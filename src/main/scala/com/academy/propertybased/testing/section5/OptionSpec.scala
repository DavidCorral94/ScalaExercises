package com.propertybased.testing.section5

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class OptionSpec
    extends AnyFunSuite
    with Matchers
    with ScalaCheckDrivenPropertyChecks {

  test("Check custom balanced Option generator") {
    forAll(Gen.listOfN(1000, CustomGenerator.balancedOptionGen)) { value =>
      val (someList, noneList) = value.partition(_.isDefined)

      someList.length shouldBe >(300)
      noneList.length shouldBe >(300)

    // We can't do this comparison because, although the list are balanced, it
    // doesn't mean that we will have the exactly amount of elements in each list
    // someList.length shouldBe noneList.length
    }
  }

}
