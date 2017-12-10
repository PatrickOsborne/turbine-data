package org.ozzysoft.turbinedata.turbine.generator

import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable

class StringGeneratorUnitTest extends FunSuite with Matchers {

  test("constant string generator") {
    val g = ConstantStringGenerator("const")
    g.stringValue shouldBe "const"

    g.next()
    g.stringValue shouldBe "const"
  }

  test("constant function string generator") {
    val g = new StringFunctionGenerator(() => "const")
    g.stringValue shouldBe "const"

    g.next()
    g.stringValue shouldBe "const"
  }

  test("function string generator with a varying function") {
    val g = TestVaryingFunctionStringGenerator.create
    g.stringValue shouldBe "1"
    g.stringValue shouldBe "2"
    g.stringValue shouldBe "3"
  }

  test("function string generator using next") {
    val g = new StringIntegerGenerator()
    g.stringValue shouldBe "0"
    g.stringValue shouldBe "0"

    g.next()
    g.stringValue shouldBe "1"

    g.next()
    g.stringValue shouldBe "2"
    g.stringValue shouldBe "2"

    g.next()
    g.stringValue shouldBe "3"
  }

  test("concatenate string generator with constants") {
    val range: immutable.Seq[Int] = 0 to 3
    val chars = range map { i => (i + 'a'.toInt).toChar }
    val generators = chars map { c => ConstantStringGenerator(c.toString) }

    val g = StringFunctionGenerator.concatenate(generators)

    g.stringValue shouldBe "abcd"
    g.stringValue shouldBe "abcd"
  }

  test("concatenate string generator varying generators") {
    val generators = Seq(new ClosedSetCharStringGenerator('a', 4), new ClosedSetCharStringGenerator('p', 4))
    val g = StringFunctionGenerator.concatenate(generators)

    g.stringValue shouldBe "ap"
    g.stringValue shouldBe "ap"

    g.next()
    g.stringValue shouldBe "bq"

    g.generate shouldBe "bq"
    g.generate shouldBe "cr"
    g.generate shouldBe "ds"
    g.generate shouldBe "ap"
  }

  test("nested concatenated string generator") {
    val generators = Seq(new ClosedSetCharStringGenerator('a', 4), new ClosedSetCharStringGenerator('p', 4))
    val g = StringFunctionGenerator.concatenate(Seq(new ClosedSetCharStringGenerator('j', 4), StringFunctionGenerator.concatenate(generators)))

    val results = List("jap", "kbq", "lcr", "mds")
    val indices = 0 to 8

    indices.foreach { i =>
      results(i % results.size) shouldBe g.generate
    }
  }
}

object TestVaryingFunctionStringGenerator {
  def create: StringFunctionGenerator = {
    val g = new TestVaryingFunctionStringGenerator()
    new StringFunctionGenerator(g.value _)
  }
}

class TestVaryingFunctionStringGenerator {
  private var counter = 0

  def value: String = {
    counter += 1
    counter.toString
  }

}

class StringIntegerGenerator(initialValue: Int = 0) extends AbstractStringGenerator {
  private var counter = initialValue

  override def next(): Unit = {
    counter += 1
  }

  def value: String = {
    counter.toString
  }

}