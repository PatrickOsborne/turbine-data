package org.ozzysoft.turbinedata.turbine.generator.core

import grizzled.slf4j.Logger
import org.scalatest.{FunSuite, Matchers}

class IntGeneratorUnitTest extends FunSuite with Matchers {

  val logger = Logger(getClass)

  test("constant int generator") {
    val g = IntConstantGenerator(1)
    g.value shouldBe 1
    g.stringValue shouldBe "1"

    g.next()
    g.value shouldBe 1
    g.stringValue shouldBe "1"
  }

  test("sum int generator with constants") {
    val g = IntConstantGenerator(1) + IntConstantGenerator(5)

    g.value shouldBe 6
    g.stringValue shouldBe "6"

    g.next()
    g.value shouldBe 6
    g.stringValue shouldBe "6"
  }

  test("int seq generator") {
    val g = new IntClosedSetGenerator(0 to 2)

    g.value shouldBe 0
    g.stringValue shouldBe "0"

    g.value shouldBe 0
    g.stringValue shouldBe "0"

    g.next()
    g.value shouldBe 1
    g.stringValue shouldBe "1"

    g.next()
    g.value shouldBe 2
    g.stringValue shouldBe "2"

    g.next()
    g.value shouldBe 0
    g.stringValue shouldBe "0"

    g.next()
    g.value shouldBe 1
    g.stringValue shouldBe "1"
  }

  test("int seq generator with generate") {
    val g = new IntClosedSetGenerator(0 to 2)

    (0 to 6) foreach { i =>
      val value = g.generate
      logger.debug(s"($i) value ($value)")
      value shouldBe (i % 3)
    }
  }

  test("sum int generator with seq generator using generate") {
    val g = IntConstantGenerator(1) + IntClosedSetGenerator(0 to 2)

    (1 to 5) foreach { loop =>
      (0 to 2) foreach { i =>
        val value = g.generate
        value shouldBe (i + 1)
      }
    }
  }

  test("sum int generator with seq using next") {
    val g = IntConstantGenerator(3) + IntClosedSetGenerator(0 to 2)

    (0 to 2) foreach { i =>
      if (i != 0) g.next()
      val value = g.value
      value shouldBe (i + 3)
    }

    (0 to 2) foreach { i =>
      g.next()
      val value = g.value
      value shouldBe (i + 3)
    }
  }

  test("sum int generators using next") {
    val g = IntFunctionGenerator.sum(Seq(IntConstantGenerator(3), IntClosedSetGenerator(0 to 2), IntClosedSetGenerator(0 to 2)))

    (0 to 2) foreach { i =>
      if (i != 0) g.next()
      val value = g.value
      value shouldBe (i + i + 3)
    }

    (0 to 2) foreach { i =>
      g.next()
      val value = g.value
      value shouldBe (i + i + 3)
    }
  }

  test("sum of nested int generators") {
    val a = IntFunctionGenerator.sum(Seq(IntConstantGenerator(7), IntClosedSetGenerator(3 to 5)))
    val g = IntFunctionGenerator.sum(Seq(IntConstantGenerator(3), IntClosedSetGenerator(0 to 2), a))

    (1 to 5) foreach { loop =>
      (0 to 2) foreach { i =>
        val value = g.generate
        value shouldBe (3 + i + 7 + 3 + i)
      }
    }
  }
}