package org.ozzysoft.turbinedata.turbine.generator.core

import grizzled.slf4j.Logger

object StringGenerators {
  def apply(s: String): StringGenerator = StringConstantGenerator(s)
}

trait ConvertsToStringGenerator {

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

  def apply(g: Generator[String]): StringFunctionGenerator = StringFunctionGenerator(() => g.value, g)

  def apply(f: () => String, generator: Generator[_]): StringFunctionGenerator = new StringFunctionGenerator(f, Seq(generator))
}

class StringFunctionGenerator(f: () => String, generators: Seq[Generator[_]] = Seq.empty) extends AbstractStringGenerator(generators) {

  def value: String = f()
}

object StringConstantGenerator {
  def apply(s: String): StringConstantGenerator = new StringConstantGenerator(s)
}

class StringConstantGenerator(s: String) extends AbstractStringGenerator() with AbstractClosedSetGenerator[String] {

  def value: String = s
}

object StringClosedSetGenerator {

  def withCallNextOnRollover(seq: Seq[String], onRolloverGenerator: Generator[_]): StringClosedSetGenerator = {
    new StringClosedSetGenerator(seq, Some(() => onRolloverGenerator.next()))
  }

  def closedSetGenerator(prefix: String, setGenerator: ClosedSetGenerator[_] with ConvertsToStringGenerator,
    maybeOnRollover: Option[() => Unit] = None): ClosedSetGenerator[String] = {
    val f = () => (new StringConstantGenerator(prefix) + setGenerator.toStringGenerator).value
    val g = new StringFunctionGenerator(f, List(setGenerator)) with AbstractClosedSetGenerator[String]
    maybeOnRollover.foreach(f => setGenerator.addRolloverListener(f))
    g
  }

}

class StringClosedSetGenerator(seq: Seq[String], maybeOnRollover: Option[() => Unit] = None)
  extends AbstractStringGenerator with AbstractClosedSetGenerator[String] {

  maybeOnRollover.foreach(f => addRolloverListener(f))

  val logger = Logger(getClass)

  private var count = 0

  override def next(): Unit = {
    count += 1
    if (count >= seq.size) {
      count = 0
      onRollover()
    }
  }

  override def value: String = {
    seq(count)
  }

}

class StringClosedSetCharGenerator(startingChar: Char, count: Int = 4, maybeOnRollover: Option[() => Unit] = None)
  extends AbstractStringGenerator with AbstractClosedSetGenerator[String] {

  maybeOnRollover.foreach(f => addRolloverListener(f))

  private var counter = 0
  private val range: Seq[Int] = 0 until count

  override def next(): Unit = {
    counter += 1
    if (counter >= range.size) {
      counter = 0
      onRollover()
    }
  }

  def value: String = {
    val index = counter % range.size
    (range(index) + startingChar.toInt).toChar.toString
  }

}