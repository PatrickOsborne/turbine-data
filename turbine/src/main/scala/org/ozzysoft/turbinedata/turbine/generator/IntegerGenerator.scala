package org.ozzysoft.turbinedata.turbine.generator

import grizzled.slf4j.Logger

object IntegerGenerator {
}

trait IntegerGenerator extends Generator[Int] {

  def +(g: IntegerGenerator): IntegerGenerator

  def intValue: Int = value

  def stringValue: String = {
    String.valueOf(value)
  }

  def toStringGenerator: StringGenerator = {
    new StringFunctionGenerator(stringValue _, this)
  }

}

abstract class AbstractIntegerGenerator(generators: Generator[_]*) extends IntegerGenerator {

  private val logger = Logger(getClass)

  def +(g: IntegerGenerator): IntegerGenerator = {
    val method = () => value + g.value
    new AbstractIntegerGenerator(Seq(this, g): _*) {
      override def value: Int = method()
    }
  }

  override def next(): Unit = {
    generators.foreach(g => g.next())
  }

}

object IntegerFunctionGenerator {

  def sum(generators: Seq[IntegerGenerator]): IntegerGenerator = {
    new IntegerFunctionGenerator(() => sumFunction(generators)().value, generators: _*)
  }

  def sumFunction(generators: Seq[IntegerGenerator]): () => IntegerGenerator = {
    () => generators.reduce((a, b) => a + b)
  }

}

class IntegerFunctionGenerator(f: () => Int, generators: Generator[_]*) extends AbstractIntegerGenerator(generators: _*) {

  override def value: Int = f()

}

object IntegerConstantGenerator {
  def apply(i: Int): IntegerConstantGenerator = new IntegerConstantGenerator(i)
}

class IntegerConstantGenerator(i: Int) extends IntegerFunctionGenerator(() => i) {}

object IntegerSequenceGenerator {
  def apply(seq: Seq[Int]): IntegerSequenceGenerator = new IntegerSequenceGenerator(seq)
}

class IntegerSequenceGenerator(seq: Seq[Int]) extends AbstractIntegerGenerator() {

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

class SimpleIntegerGenerator(initialValue: Int = 0) extends AbstractIntegerGenerator {
  private var counter = initialValue

  override def next(): Unit = {
    counter += 1
  }

  def value: Int = {
    counter
  }

}