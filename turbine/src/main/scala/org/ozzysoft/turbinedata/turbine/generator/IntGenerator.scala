package org.ozzysoft.turbinedata.turbine.generator

import grizzled.slf4j.Logger

object IntGenerator {
  def apply(x: Int): IntGenerator = IntConstantGenerator(x)
}

trait IntGenerator extends Generator[Int] with StringGeneratorLike {

  def +(g: IntGenerator): IntGenerator

  def *(g: IntGenerator): IntGenerator

  def intValue: Int = value

  def stringValue: String = {
    String.valueOf(value)
  }

  def toStringGenerator: StringGenerator = {
    StringFunctionGenerator(stringValue _, this)
  }

}

abstract class AbstractIntGenerator(generators: Seq[Generator[_]] = Seq.empty) extends IntGenerator {

  private val logger = Logger(getClass)

  def +(g: IntGenerator): IntGenerator = {
    val method = () => value + g.value
    new AbstractIntGenerator(Seq(this, g)) {
      override def value: Int = method()
    }
  }

  def *(g: IntGenerator): IntGenerator = {
    val method = () => value * g.value
    new AbstractIntGenerator(Seq(this, g)) {
      override def value: Int = method()
    }
  }

  override def next(): Unit = {
    generators.foreach(g => g.next())
  }

}

object IntFunctionGenerator {

  def sum(generators: Seq[IntGenerator]): IntGenerator = {
    new IntFunctionGenerator(() => sumFunction(generators)().value, generators)
  }

  def sumFunction(generators: Seq[IntGenerator]): () => IntGenerator = {
    () => generators.reduce((a, b) => a + b)
  }

  def apply(f: () => Int, generator: Generator[_]): IntFunctionGenerator = new IntFunctionGenerator(f, Seq(generator))
}

class IntFunctionGenerator(f: () => Int, generators: Seq[Generator[_]] = Seq.empty) extends AbstractIntGenerator(generators: Seq[Generator[_]]) {

  override def value: Int = f()

}

object IntConstantGenerator {
  def apply(i: Int): IntConstantGenerator = new IntConstantGenerator(i)
}

class IntConstantGenerator(i: Int) extends IntFunctionGenerator(() => i) {}

object IntClosedSetGenerator {
  def apply(seq: Seq[Int]): IntClosedSetGenerator = new IntClosedSetGenerator(seq)
}

class IntClosedSetGenerator(seq: Seq[Int]) extends AbstractIntGenerator() {

  private val logger = Logger(getClass)

  private var count = 0

  override def next(): Unit = {
    if (count >= (seq.size - 1)) {
      count = 0
    }
    else count += 1
  }

  override def value: Int = {
    seq(count)
  }
}

class SimpleIntGenerator(initialValue: Int = 0) extends AbstractIntGenerator {
  private var counter = initialValue

  override def next(): Unit = {
    counter += 1
  }

  def value: Int = {
    counter
  }

}