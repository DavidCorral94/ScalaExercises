package com.academy.propertybased.testing.section5

import org.scalacheck.Gen

object CustomGenerator {

  def balancedOptionGen: Gen[Option[Int]] =
    for {
      isSome <- Gen.oneOf(true, false)
      res <- if (isSome) Gen.option(Gen.choose(1, 100)) else Gen.const(None)
    } yield res
}
