package org.ozzysoft.turbinedata.turbine.generator

object Generators {

  def apply(s: String): StringGenerator = StringGenerator(s)

  def apply(i: Int): IntGenerator = IntGenerator(i)

  def apply(objects: Seq[Any]): Seq[Generator[_]] = {
    objects map {
      case s: String => Generators(s)
      case i: Int => Generators(i)
      case g: Generator[_] => g
      case x => throw new RuntimeException(s"unable to map object to generator ($x)")
    }
  }

  def stringGenerators(objects: Seq[Any]): Seq[StringGenerator] = {
    Generators(objects) map {
      case i: IntGenerator => i.toStringGenerator
      case g: StringGeneratorLike => g.toStringGenerator
      case x => throw new RuntimeException(s"unable to map object to generator ($x)")
    }
  }

}
