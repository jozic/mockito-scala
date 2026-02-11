package org.mockito.captor

import scala.quoted.*
import scala.reflect.ClassTag

/**
 * Scala 3 version - trait extends shared CaptorBase. Companion object must be in same file (Scala requirement).
 */
trait Captor[T] extends CaptorBase[T]

/** WrapperCaptor for Scala 3 - extends both WrapperCaptorBase and Captor trait */
class WrapperCaptor[T: ClassTag] extends WrapperCaptorBase[T] with Captor[T]

/** Value class captor: captures the underlying type U and wraps into T */
class ValueClassCaptor[T, U](underlyingCt: ClassTag[U])(using wrap: U => T) extends Captor[T] {
  import scala.jdk.CollectionConverters.*

  private val argumentCaptor = org.mockito.ArgumentCaptor.forClass(org.mockito.clazz[U](using underlyingCt))

  override def capture: T      = wrap(argumentCaptor.capture())
  override def value: T        = wrap(argumentCaptor.getValue)
  override def values: List[T] = argumentCaptor.getAllValues.asScala.map(wrap).toList
}

object Captor {
  given asCapture[T]: Conversion[Captor[T], T] with {
    def apply(c: Captor[T]): T = c.capture
  }

  inline given materializeValueClassCaptor[T]: Captor[T] = ${ materializeValueClassCaptorImpl[T] }

  def materializeValueClassCaptorImpl[T: Type](using Quotes): Expr[Captor[T]] = {
    import quotes.reflect.*

    val tpe        = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol

    // Check if this is a user-defined value class (extends AnyVal, not a primitive)
    val primitives   = Set("scala.Int", "scala.Long", "scala.Double", "scala.Float", "scala.Boolean", "scala.Byte", "scala.Short", "scala.Char", "scala.Unit")
    val isValueClass = tpe.baseClasses.exists(_.fullName == "scala.AnyVal") &&
      typeSymbol != Symbol.classSymbol("scala.AnyVal") &&
      !primitives.contains(typeSymbol.fullName)

    if isValueClass then {
      // Find the primary constructor's single parameter type (the underlying type)
      val constructor = typeSymbol.primaryConstructor
      val paramType   = constructor.paramSymss.flatten
        .collectFirst {
          case p if p.isTerm => tpe.memberType(p).widen
        }
        .getOrElse(report.errorAndAbort(s"Could not find constructor parameter for value class ${tpe.show}"))

      paramType.asType match {
        case '[u] =>
          Expr.summon[ClassTag[u]] match {
            case Some(ct) =>
              '{ new ValueClassCaptor[T, u]($ct)(using ${ constructValueClass[T, u] }) }
            case None =>
              report.errorAndAbort(s"No ClassTag available for underlying type ${paramType.show}")
          }
      }
    } else {
      Expr.summon[ClassTag[T]] match {
        case Some(ct) =>
          '{ new WrapperCaptor[T](using $ct) }
        case None =>
          report.errorAndAbort(s"No ClassTag available for ${tpe.show}")
      }
    }
  }

  /** Create a function expression that wraps an underlying value into its value class */
  private def constructValueClass[T: Type, U: Type](using Quotes): Expr[U => T] = {
    import quotes.reflect.*
    val tpe         = TypeRepr.of[T]
    val constructor = tpe.typeSymbol.primaryConstructor

    '{ (v: U) => ${ New(TypeTree.of[T]).select(constructor).appliedTo('v.asTerm).asExprOf[T] } }
  }
}
