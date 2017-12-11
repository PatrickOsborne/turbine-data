package org.ozzysoft.turbinedata.turbine.generator

class CsvGenerator(generators: Seq[Generator[String]]) extends AbstractStringGenerator(generators: _*) {

  override def value: String = {
    val strings = generators map { g => s"${g.value}" }
    strings.mkString(", ")
  }

}
