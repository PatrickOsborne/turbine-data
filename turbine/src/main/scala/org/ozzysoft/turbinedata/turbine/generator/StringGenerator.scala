package org.ozzysoft.turbinedata.turbine.generator

import grizzled.slf4j.Logger

object StringGenerator {
  def apply(s: String): StringGenerator = StringConstantGenerator(s)
}

trait StringGenerator extends Generator[String] {

  def stringValue: String = value

  def +(g: StringGenerator): StringGenerator

}

trait StringGeneratorLike {

  def toStringGenerator: StringGenerator

}

abstract class AbstractStringGenerator(generators: Seq[Generator[_]] = Seq.empty) extends StringGenerator {

  override def next(): Unit = generators.foreach(g => g.next())

  def +(g: StringGenerator): StringGenerator = {
    val method: () => String = () => value + g.value
    new AbstractStringGenerator(Seq(this, g)) {
      override def value: String = method()
    }
  }
}

object StringFunctionGenerator {

  def concatenateFunction(generators: Seq[StringGenerator]): () => StringGenerator = {
    () => generators.reduce((a, b) => a + b)
  }

  def concatenate(generators: Seq[StringGenerator]): StringFunctionGenerator = {
    new StringFunctionGenerator(() => concatenateFunction(generators)().value, generators)
  }

  def apply(f: () => String, generator: Generator[_]): StringFunctionGenerator = new StringFunctionGenerator(f, Seq(generator))
}

class StringFunctionGenerator(f: () => String, generators: Seq[Generator[_]] = Seq.empty) extends AbstractStringGenerator(generators) {

  def value: String = f()
}

object StringConstantGenerator {
  def apply(s: String): StringConstantGenerator = new StringConstantGenerator(s)
}

class StringConstantGenerator(s: String) extends AbstractStringGenerator() {

  def value: String = s
}

class StringClosedSetGenerator(seq: Seq[String]) extends AbstractStringGenerator() {

  val logger = Logger(getClass)

  private var count = 0

  override def next(): Unit = {
    if (count >= (seq.size - 1)) {
      count = 0
    }
    else count += 1
  }

  override def value: String = {
    seq(count)
  }
}

class ClosedSetCharStringGenerator(startingChar: Char, count: Int = 4) extends AbstractStringGenerator {
  private var counter = 0
  private val range: Seq[Int] = 0 until count

  override def next(): Unit = {
    counter += 1
  }

  def value: String = {
    val index = counter % range.size
    (range(index) + startingChar.toInt).toChar.toString
  }

}