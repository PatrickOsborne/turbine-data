package org.ozzysoft.turbinedata.turbine.generator.core

class CsvGenerator(generators: Seq[Generator[String]]) extends AbstractStringGenerator(generators) {

  override def value: String = {
    val strings = generators map { g => s"${g.value}" }
    strings.mkString(", ")
  }

}
