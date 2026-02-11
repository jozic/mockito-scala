package org.mockito.internal

import scala.quoted.*

/**
 * Scala 3 macro implementation for ValueClassWrapper.
 */
trait ValueClassWrapperCompat {
  inline implicit def instance[VC]: ValueClassWrapper[VC] = ${ ValueClassWrapperMacro.materialise[VC] }
}

object ValueClassWrapperMacro {
  def materialise[VC: Type](using Quotes): Expr[ValueClassWrapper[VC]] = {
    import quotes.reflect.*

    val tpe        = TypeRepr.of[VC]
    val typeSymbol = tpe.typeSymbol

    // Check if this is a value class (extends AnyVal and is not a primitive)
    // Value classes can be case classes or regular classes
    val isValueClass = tpe.baseClasses.exists(_.fullName == "scala.AnyVal") &&
      !typeSymbol.flags.is(Flags.Abstract) &&
      typeSymbol.isClassDef

    if isValueClass then {
      Expr.summon[scala.reflect.ClassTag[VC]] match {
        case Some(ct) =>
          '{
            given scala.reflect.ClassTag[VC] = $ct
            new ReflectionWrapper[VC]
          }
        case None =>
          report.errorAndAbort(s"No ClassTag available for ${tpe.show}")
      }
    } else {
      '{ new NormalClassWrapper[VC] }
    }
  }
}
