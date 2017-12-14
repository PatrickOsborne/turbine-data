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

trait ClosedSetGenerator[T] extends Generator[T] {

  protected val maybeOnRollover: Option[() => Unit] = None

  protected def onRollover(): Unit = {
    maybeOnRollover.foreach(f => f())
  }

}