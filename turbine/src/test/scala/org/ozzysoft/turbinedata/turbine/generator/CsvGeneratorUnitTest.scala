package org.ozzysoft.turbinedata.turbine.generator

import java.io.{BufferedReader, StringReader}

import grizzled.slf4j.Logger
import org.scalatest.{FunSuite, Matchers}

class CsvGeneratorUnitTest extends FunSuite with Matchers {

  val logger = Logger(getClass)

  test("generate csv with next()") {
    val a1 = new StringClosedSetCharGenerator('a', 2)
    val a2 = new StringClosedSetCharGenerator('b', 2)
    val a3 = new StringClosedSetCharGenerator('c', 2)
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

  test("generate csv with generate()") {
    val a1 = new StringClosedSetCharGenerator('a', 2)
    val a2 = new StringClosedSetCharGenerator('b', 2)
    val a3 = new StringClosedSetCharGenerator('c', 2)
    val generators = Seq(a1, a2, a3)

    val g = new CsvGenerator(generators)

    g.generate shouldBe "a, b, c"
    logger.info(g.value)
    g.generate shouldBe "b, c, d"
    logger.info(g.value)
    g.generate shouldBe "a, b, c"
    logger.info(g.value)
  }

  test("generate csv with rollover") {
    val expectedResult = "user-1, 1, 2, 11\nuser-2, 2, 4, 12\nuser-3, 3, 6, 13\nuser-4, 4, 8, 14\nuser-1, 5, 10, 15\nuser-2, 6, 12, 16\nuser-3, 7, 14, 17\nuser-4, 8, 16, 18"
    val expectedResults = readToList(new BufferedReader(new StringReader(expectedResult)))

    val userNumberGenerator = new IntClosedSetGenerator(1 to 4)
    val userGenerator = StringClosedSetGenerator.closedSetGenerator("user-", userNumberGenerator)

    val id = new SimpleIntGenerator(1)
    val counter = IntGenerators.wrapperWithoutNext(id)

    val generators = Seq(userGenerator, id, IntConstantGenerator(2) * counter, IntConstantGenerator(10) + counter)
    val stringGenerators = Generators.stringGenerators(generators)

    val g = new CsvGenerator(stringGenerators)

    (1 to 8) foreach { i =>
      val value = g.generate
      logger.info(s"$value")
      value shouldBe expectedResults(i - 1)
    }
  }

  def readToList(reader: BufferedReader): List[String] = {
    try {
      var list: List[String] = List.empty

      var done = false
      while (!done) {
        val line = reader.readLine()
        if (line == null) done = true
        else list = line +: list
      }

      list.reverse
    }
    finally {
      reader.close()
    }
  }

}
