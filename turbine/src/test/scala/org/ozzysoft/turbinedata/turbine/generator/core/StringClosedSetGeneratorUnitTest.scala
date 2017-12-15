package org.ozzysoft.turbinedata.turbine.generator.core

import org.scalatest.{FunSuite, Matchers}

class StringClosedSetGeneratorUnitTest extends FunSuite with Matchers {

  test("closed set generator") {
    val g = new StringClosedSetGenerator((0 to 1).map(i => i.toString))
    (0 to 6).foreach { i =>
      val value = i % 2
      g.generate shouldBe value.toString
    }
  }

  test("closed set generator with rollover handler") {
    val rolloverCounter = new RolloverCounter()
    rolloverCounter.counter shouldBe 0

    val g = new StringClosedSetGenerator((0 to 1).map(i => i.toString), Some(rolloverCounter.onRollover _))
    (0 to 6).foreach { i =>
      val value = i % 2
      g.generate shouldBe value.toString
      if (i != 0 && value == 0) rolloverCounter.counter shouldBe (i / 2)
    }

    rolloverCounter.counter shouldBe 3
  }

  test("closed set generator with next on rollover handler") {
    val rolloverGenerator = new SimpleIntGenerator()
    val g = StringClosedSetGenerator.withCallNextOnRollover((0 to 1).map(i => i.toString), rolloverGenerator)

    rolloverGenerator.value shouldBe 0

    (0 to 6).foreach { i =>
      val value = i % 2
      g.generate shouldBe value.toString
      if (i != 0 && value == 0) rolloverGenerator.value shouldBe (i / 2)
    }

    rolloverGenerator.value shouldBe 3
  }

}

class RolloverCounter {

  var counter = 0

  def onRollover(): Unit = {
    counter += 1
  }
}
