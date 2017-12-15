package org.ozzysoft.turbinedata.turbine.generator

object Generator {
}

trait Generator[T] {

  protected var first = true

  def value: T

  def next(): Unit = {}

  def generate: T = {
    generateNext()
    value
  }

  protected def generateNext(): Unit = {
    if (first) first = false
    else next()
  }

}

trait StringGenerator extends Generator[String] {

  def stringValue: String = value

  def +(g: StringGenerator): StringGenerator

}

trait IntGenerator extends Generator[Int] with ConvertsToStringGenerator {

  def +(g: IntGenerator): IntGenerator

  def *(g: IntGenerator): IntGenerator

  def intValue: Int = value

  def stringValue: String = {
    String.valueOf(value)
  }

}

trait LongGenerator extends Generator[Long] with ConvertsToStringGenerator {

  def +(g: IntGenerator): LongGenerator

  def *(g: IntGenerator): LongGenerator

  def longValue: Long = value

  def stringValue: String = {
    String.valueOf(value)
  }

}

trait FloatGenerator extends Generator[Float] with ConvertsToStringGenerator {

  def +(g: IntGenerator): FloatGenerator

  def *(g: IntGenerator): FloatGenerator

  def floatValue: Float = value

  def stringValue: String = {
    String.valueOf(value)
  }

}

trait DoubleGenerator extends Generator[Double] with ConvertsToStringGenerator {

  def +(g: IntGenerator): DoubleGenerator

  def *(g: IntGenerator): DoubleGenerator

  def doubleValue: Double = value

  def stringValue: String = {
    String.valueOf(value)
  }

}

trait ClosedSetGenerator[T] extends Generator[T] {

  def addRolloverListener(f: () => Unit): Unit

}

trait AbstractClosedSetGenerator[T] extends ClosedSetGenerator[T] {

  protected var rolloverListeners: Seq[() => Unit] = List.empty

  override def addRolloverListener(f: () => Unit): Unit = {
    rolloverListeners = f +: rolloverListeners
  }

  protected def onRollover(): Unit = {
    rolloverListeners.foreach(f => f())
  }

}