package org.ozzysoft.turbinedata.turbine.generator

import grizzled.slf4j.Logger
import org.scalatest.{FunSuite, Matchers}

class CsvGeneratorUnitTest extends FunSuite with Matchers {

  val logger = Logger(getClass)

  test("generate csv") {
    val a1 = new ClosedSetCharStringGenerator('a', 2)
    val a2 = new ClosedSetCharStringGenerator('b', 2)
    val a3 = new ClosedSetCharStringGenerator('c', 2)
    val generators = Seq(a1, a2, a3)

    val g = new CsvGenerator(generators)

    logger.info(g.value)
    g.value shouldBe "a, b, c"

    g.next()
    logger.info(g.value)
    g.value shouldBe "b, c, d"

    g.next()
    logger.info(g.value)
    g.value shouldBe "a, b, c"
  }

}
