package org.mockito.matchers

import scala.quoted.*

/**
 * Scala 3 macro implementation for DefaultValueProvider.
 */
trait DefaultValueProviderCompat {
  inline implicit def default[T]: DefaultValueProvider[T] = ${ DefaultValueProviderMacro.defaultValueProviderImpl[T] }
}

object DefaultValueProviderMacro {
  def defaultValueProviderImpl[T: Type](using Quotes): Expr[DefaultValueProvider[T]] = {
    import quotes.reflect.*

    val tpe        = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol

    // Check if this is a value class (extends AnyVal and is a case class)
    val isValueClass = tpe.baseClasses.exists(_.fullName == "scala.AnyVal") &&
      typeSymbol.flags.is(Flags.Case)

    if isValueClass then {
      // For value classes, we need to create an instance with a default value for the wrapped type
      // Get the primary constructor parameter type
      val primaryConstructor = typeSymbol.primaryConstructor

      // Get the first (and only) value parameter type (skip type parameters)
      val paramTypeOpt: Option[TypeRepr] = primaryConstructor.paramSymss.collectFirst {
        case params if params.nonEmpty =>
          // Filter out type parameters (TypeDef) and only get value parameters (ValDef)
          params.collectFirst {
            case v if v.tree match {
                  case _: ValDef => true
                  case _         => false
                } =>
              v.tree match {
                case valDef: ValDef => valDef.tpt.tpe
              }
          }
      }.flatten

      paramTypeOpt match {
        case Some(innerType) =>
          innerType.asType match {
            case '[innerT] =>
              // Generate code that constructs the value class with a default inner value
              // Build: new T(DefaultValueProvider.defaultProvider[innerT].default)

              // Get the type class to get its runtime class
              val typeClass = Ref(typeSymbol.companionModule)

              // Build the constructor call tree
              val defaultProviderModule = Symbol.requiredModule("org.mockito.matchers.DefaultValueProvider")
              val innerDefaultExpr      = '{
                DefaultValueProvider.defaultProvider[innerT].default
              }

              // Build: new T(innerDefault)
              val constructorCall = Apply(
                Select(New(TypeIdent(typeSymbol)), primaryConstructor),
                List(innerDefaultExpr.asTerm)
              )

              val providerExpr = '{
                new DefaultValueProvider[T] {
                  override def default: T = ${ constructorCall.asExprOf[T] }
                }
              }

              providerExpr
          }
        case None =>
          '{ DefaultValueProvider.defaultProvider[T] }
      }
    } else {
      // For regular types, use the default provider
      '{ DefaultValueProvider.defaultProvider[T] }
    }
  }
}
