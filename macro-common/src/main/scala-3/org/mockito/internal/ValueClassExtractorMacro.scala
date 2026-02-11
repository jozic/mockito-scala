package org.mockito.internal

import scala.quoted.*

/**
 * Scala 3 macro implementation for ValueClassExtractor.
 */
trait ValueClassExtractorCompat {
  inline implicit def instance[VC]: ValueClassExtractor[VC] = ${ ValueClassExtractorMacro.materialise[VC] }
}

object ValueClassExtractorMacro {
  def materialise[VC: Type](using Quotes): Expr[ValueClassExtractor[VC]] = {
    import quotes.reflect.*

    val tpe        = TypeRepr.of[VC]
    val typeSymbol = tpe.typeSymbol

    // Check if this is a value class (extends AnyVal and is not a primitive)
    // Value classes can be case classes or regular classes
    val isValueClass = tpe.baseClasses.exists(_.fullName == "scala.AnyVal") &&
      !typeSymbol.flags.is(Flags.Abstract) &&
      typeSymbol.isClassDef

    if isValueClass then {
      '{ new ReflectionExtractor[VC] }
    } else {
      '{ new NormalClassExtractor[VC] }
    }
  }
}
