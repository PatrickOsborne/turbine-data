package org.ozzysoft.turbinedata.turbine.generator

import grizzled.slf4j.Logger

object IntGenerator {
}

trait IntGenerator extends Generator[Int] {

  def +(g: IntGenerator): IntGenerator

  def intValue: Int = value

  def stringValue: String = {
    String.valueOf(value)
  }

  def toStringGenerator: StringGenerator = {
    new StringFunctionGenerator(stringValue _, this)
  }

}

abstract class AbstractIntGenerator(generators: Generator[_]*) extends IntGenerator {

  private val logger = Logger(getClass)

  def +(g: IntGenerator): IntGenerator = {
    val method = () => value + g.value
    new AbstractIntGenerator(Seq(this, g): _*) {
      override def value: Int = method()
    }
  }

  override def next(): Unit = {
    generators.foreach(g => g.next())
  }

}

object IntFunctionGenerator {

  def sum(generators: Seq[IntGenerator]): IntGenerator = {
    new IntFunctionGenerator(() => sumFunction(generators)().value, generators: _*)
  }

  def sumFunction(generators: Seq[IntGenerator]): () => IntGenerator = {
    () => generators.reduce((a, b) => a + b)
  }

}

class IntFunctionGenerator(f: () => Int, generators: Generator[_]*) extends AbstractIntGenerator(generators: _*) {

  override def value: Int = f()

}

object IntConstantGenerator {
  def apply(i: Int): IntConstantGenerator = new IntConstantGenerator(i)
}

class IntConstantGenerator(i: Int) extends IntFunctionGenerator(() => i) {}

object IntSequenceGenerator {
  def apply(seq: Seq[Int]): IntSequenceGenerator = new IntSequenceGenerator(seq)
}

class IntSequenceGenerator(seq: Seq[Int]) extends AbstractIntGenerator() {

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