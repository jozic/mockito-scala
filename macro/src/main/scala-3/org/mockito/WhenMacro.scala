package org.mockito

import org.mockito.Utils.*
import org.mockito.WhenDslKeywords.*
import org.mockito.WhenMacroRuntime.{ AnswerActions, AnswerPFActions, RealMethod }
import org.mockito.stubbing.{ OngoingStubbing, ScalaFirstStubbing, ScalaOngoingStubbing }
import org.scalactic.Prettifier

import scala.quoted.*

/**
 * Scala 3 macro implementations for the idiomatic when/stubbing DSL. Runtime support classes are in WhenMacroRuntime.
 */
object WhenMacro {
  // Re-export runtime classes for backward compatibility
  type AnswerActions[T]   = WhenMacroRuntime.AnswerActions[T]
  type AnswerPFActions[T] = WhenMacroRuntime.AnswerPFActions[T]
  val RealMethod = WhenMacroRuntime.RealMethod

  /**
   * Transform a method invocation by wrapping non-matcher arguments in DefaultMatcher
   */
  private def transformInvocation(using
      Quotes
  )(
      invocation: quotes.reflect.Term,
      hoisted: scala.collection.mutable.ListBuffer[quotes.reflect.Statement],
      matcherValNames: Set[String] = Set.empty
  ): quotes.reflect.Term = {
    import quotes.reflect.*

    invocation match {
      // Handle inlined expressions (from Scala 3 inline expansion)
      case Inlined(call, bindings, expansion) =>
        Inlined(call, bindings, transformInvocation(expansion, hoisted, matcherValNames))

      // Handle blocks with hoisted named args (Scala 3 desugars named args into val defs)
      case Block(stats, expr) =>
        val detectedMatcherVals         = detectMatcherValDefs(stats)
        val (remainingStats, fixedExpr) = inlineMatcherValDefs(stats, expr, detectedMatcherVals)
        val allMatcherVals              = matcherValNames ++ detectedMatcherVals
        val transformed                 = transformInvocation(fixedExpr, hoisted, allMatcherVals)
        if remainingStats.nonEmpty then Block(remainingStats, transformed) else transformed

      // Match: obj.method[TypeArgs](args1)(args2)...
      case Apply(fun, args) =>
        val transformedArgs = transformArgsForApply(fun, args, hoisted, matcherValNames)
        Apply(transformInvocation(fun, hoisted, matcherValNames), transformedArgs)

      // Match: obj.method[TypeArgs]
      case TypeApply(fun, targs) =>
        TypeApply(transformInvocation(fun, hoisted, matcherValNames), targs)

      // Match: obj.method (no args, no type args) or base case
      case _ => invocation
    }
  }

  private def doTransformInvocation(using Quotes)(invocation: quotes.reflect.Term): quotes.reflect.Term = {
    import quotes.reflect.*
    val hoisted     = scala.collection.mutable.ListBuffer.empty[Statement]
    val transformed = transformInvocation(invocation, hoisted)
    if hoisted.nonEmpty then Block(hoisted.toList, transformed) else transformed
  }

  // Raw macro: transforms arguments and wraps in Mockito.when, returns OngoingStubbing[T]
  // Used by cats/scalaz modules to build their own action wrappers
  inline def whenRaw[T](inline stubbing: T): OngoingStubbing[T] =
    ${ whenRawMacro[T]('stubbing) }

  def whenRawMacro[T: Type](stubbing: Expr[T])(using Quotes): Expr[OngoingStubbing[T]] = {
    import quotes.reflect.*
    val transformed = doTransformInvocation(stubbing.asTerm)
    '{ org.mockito.Mockito.when[T](${ transformed.asExprOf[T] }) }
  }

  // Macro methods for IdiomaticStubbing extension methods (return action objects)
  inline def shouldReturn[T](inline stubbing: T): Any =
    ${ shouldReturnMacro[T]('stubbing) }

  def shouldReturnMacro[T: Type](stubbing: Expr[T])(using Quotes): Expr[Any] = {
    import quotes.reflect.*

    val invocation  = stubbing.asTerm
    val transformed = doTransformInvocation(invocation)

    // Build: new org.mockito.IdiomaticMockitoBaseRuntime.ReturnActions[T](new ScalaFirstStubbing[T](Mockito.when[T](transformed)))
    val returnActionsClass      = Symbol.requiredClass("org.mockito.IdiomaticMockitoBaseRuntime.ReturnActions")
    val scalaFirstStubbingClass = Symbol.requiredClass("org.mockito.stubbing.ScalaFirstStubbing")
    val mockitoClass            = Symbol.requiredModule("org.mockito.Mockito")

    val whenCall = TypeApply(
      Select.unique(Ref(mockitoClass), "when"),
      List(TypeTree.of[T])
    ).appliedTo(transformed)

    // Wrap in ScalaFirstStubbing: new ScalaFirstStubbing[T](whenCall)
    val scalaStubbing = TypeApply(
      Select(New(TypeTree.ref(scalaFirstStubbingClass)), scalaFirstStubbingClass.primaryConstructor),
      List(TypeTree.of[T])
    ).appliedTo(whenCall)

    // Build: new ReturnActions[T](scalaStubbing)
    val newExpr = TypeApply(
      Select(New(TypeTree.ref(returnActionsClass)), returnActionsClass.primaryConstructor),
      List(TypeTree.of[T])
    ).appliedTo(scalaStubbing)

    newExpr.asExprOf[Any]
  }

  inline def shouldThrow[T](inline stubbing: T): Any =
    ${ shouldThrowMacro[T]('stubbing) }

  def shouldThrowMacro[T: Type](stubbing: Expr[T])(using Quotes): Expr[Any] = {
    import quotes.reflect.*

    val invocation  = stubbing.asTerm
    val transformed = doTransformInvocation(invocation)

    // Build: new org.mockito.IdiomaticMockitoBaseRuntime.ThrowActions[T](new ScalaFirstStubbing[T](Mockito.when[T](transformed)))
    val throwActionsClass       = Symbol.requiredClass("org.mockito.IdiomaticMockitoBaseRuntime.ThrowActions")
    val scalaFirstStubbingClass = Symbol.requiredClass("org.mockito.stubbing.ScalaFirstStubbing")
    val mockitoClass            = Symbol.requiredModule("org.mockito.Mockito")

    val whenCall = TypeApply(
      Select.unique(Ref(mockitoClass), "when"),
      List(TypeTree.of[T])
    ).appliedTo(transformed)

    // Wrap in ScalaFirstStubbing: new ScalaFirstStubbing[T](whenCall)
    val scalaStubbing = TypeApply(
      Select(New(TypeTree.ref(scalaFirstStubbingClass)), scalaFirstStubbingClass.primaryConstructor),
      List(TypeTree.of[T])
    ).appliedTo(whenCall)

    // Build: new ThrowActions[T](scalaStubbing)
    val newExpr = TypeApply(
      Select(New(TypeTree.ref(throwActionsClass)), throwActionsClass.primaryConstructor),
      List(TypeTree.of[T])
    ).appliedTo(scalaStubbing)

    newExpr.asExprOf[Any]
  }

  inline def shouldAnswer[T](inline stubbing: T): Any =
    ${ shouldAnswerMacro[T]('stubbing) }

  def shouldAnswerMacro[T: Type](stubbing: Expr[T])(using Quotes): Expr[Any] = {
    import quotes.reflect.*

    val invocation  = stubbing.asTerm
    val transformed = doTransformInvocation(invocation)

    // Build: new WhenMacroRuntime.AnswerActions[T](new ScalaFirstStubbing[T](Mockito.when[T](transformed)))
    val answerActionsClass      = Symbol.requiredClass("org.mockito.WhenMacroRuntime.AnswerActions")
    val scalaFirstStubbingClass = Symbol.requiredClass("org.mockito.stubbing.ScalaFirstStubbing")
    val mockitoClass            = Symbol.requiredModule("org.mockito.Mockito")

    val whenCall = TypeApply(
      Select.unique(Ref(mockitoClass), "when"),
      List(TypeTree.of[T])
    ).appliedTo(transformed)

    // Wrap in ScalaFirstStubbing: new ScalaFirstStubbing[T](whenCall)
    val scalaStubbing = TypeApply(
      Select(New(TypeTree.ref(scalaFirstStubbingClass)), scalaFirstStubbingClass.primaryConstructor),
      List(TypeTree.of[T])
    ).appliedTo(whenCall)

    // Build: new AnswerActions[T](scalaStubbing)
    val newExpr = TypeApply(
      Select(New(TypeTree.ref(answerActionsClass)), answerActionsClass.primaryConstructor),
      List(TypeTree.of[T])
    ).appliedTo(scalaStubbing)

    newExpr.asExprOf[Any]
  }

  inline def shouldAnswerPF[T](inline stubbing: T): Any =
    ${ shouldAnswerPFMacro[T]('stubbing) }

  def shouldAnswerPFMacro[T: Type](stubbing: Expr[T])(using Quotes): Expr[Any] = {
    import quotes.reflect.*

    val invocation  = stubbing.asTerm
    val transformed = doTransformInvocation(invocation)

    // Build: new WhenMacroRuntime.AnswerPFActions[T](new ScalaFirstStubbing[T](Mockito.when[T](transformed)))
    val answerPFActionsClass    = Symbol.requiredClass("org.mockito.WhenMacroRuntime.AnswerPFActions")
    val scalaFirstStubbingClass = Symbol.requiredClass("org.mockito.stubbing.ScalaFirstStubbing")
    val mockitoClass            = Symbol.requiredModule("org.mockito.Mockito")

    val whenCall = TypeApply(
      Select.unique(Ref(mockitoClass), "when"),
      List(TypeTree.of[T])
    ).appliedTo(transformed)

    // Wrap in ScalaFirstStubbing: new ScalaFirstStubbing[T](whenCall)
    val scalaStubbing = TypeApply(
      Select(New(TypeTree.ref(scalaFirstStubbingClass)), scalaFirstStubbingClass.primaryConstructor),
      List(TypeTree.of[T])
    ).appliedTo(whenCall)

    // Build: new AnswerPFActions[T](scalaStubbing)
    val newExpr = TypeApply(
      Select(New(TypeTree.ref(answerPFActionsClass)), answerPFActionsClass.primaryConstructor),
      List(TypeTree.of[T])
    ).appliedTo(scalaStubbing)

    newExpr.asExprOf[Any]
  }

  inline def shouldReturn[T, R](inline stubbing: => T, inline v: => R)(using $pt: Prettifier): OngoingStubbing[T] =
    ${ shouldReturnImpl[T, R]('stubbing, 'v, '$pt) }

  def shouldReturnImpl[T: Type, R: Type](
      stubbing: Expr[T],
      v: Expr[R],
      pt: Expr[Prettifier]
  )(using Quotes): Expr[OngoingStubbing[T]] = {
    import quotes.reflect.*

    val invocation  = stubbing.asTerm
    val transformed = doTransformInvocation(invocation)

    // Build: new org.mockito.IdiomaticMockitoBaseRuntime.ReturnActions[T](Mockito.when[T](transformed))
    val returnActionsClass = Symbol.requiredClass("org.mockito.IdiomaticMockitoBaseRuntime.ReturnActions")
    val mockitoClass       = Symbol.requiredModule("org.mockito.Mockito")
    val whenMethod         = mockitoClass.methodMember("when").head

    val whenCall = TypeApply(
      Select.unique(Ref(mockitoClass), "when"),
      List(TypeTree.of[T])
    ).appliedTo(transformed)

    Apply(
      Select.overloaded(New(TypeTree.ref(returnActionsClass)), "<init>", List(TypeRepr.of[T]), Nil),
      List(whenCall)
    ).asExprOf[OngoingStubbing[T]]
  }

  inline def isLenient[T](inline stubbing: => T)(using $pt: Prettifier): ScalaFirstStubbing[T] =
    ${ isLenientImpl[T]('stubbing, '$pt) }

  def isLenientImpl[T: Type](
      stubbing: Expr[T],
      pt: Expr[Prettifier]
  )(using Quotes): Expr[ScalaFirstStubbing[T]] = {
    import quotes.reflect.*

    val invocation  = stubbing.asTerm
    val transformed = doTransformInvocation(invocation)

    '{
      val s = new ScalaFirstStubbing[T](
        org.mockito.Mockito.when[T](${ transformed.asExprOf[T] })
      )
      s.isLenient()
      s
    }
  }

  inline def shouldCallRealMethod[T](inline stubbing: => T)(using $pt: Prettifier): ScalaOngoingStubbing[T] =
    ${ shouldCallRealMethodImpl[T]('stubbing, '$pt) }

  def shouldCallRealMethodImpl[T: Type](
      stubbing: Expr[T],
      pt: Expr[Prettifier]
  )(using Quotes): Expr[ScalaOngoingStubbing[T]] = {
    import quotes.reflect.*

    val invocation  = stubbing.asTerm
    val transformed = doTransformInvocation(invocation)

    '{
      new ScalaOngoingStubbing[T](
        org.mockito.Mockito.when[T](${ transformed.asExprOf[T] }).thenCallRealMethod()
      )
    }
  }

  inline def shouldThrow[T](inline stubbing: => T, inline throwables: Throwable*)(using $pt: Prettifier): OngoingStubbing[T] =
    ${ shouldThrowImpl[T]('stubbing, 'throwables, '$pt) }

  def shouldThrowImpl[T: Type](
      stubbing: Expr[T],
      throwables: Expr[Seq[Throwable]],
      pt: Expr[Prettifier]
  )(using Quotes): Expr[OngoingStubbing[T]] = {
    import quotes.reflect.*

    val invocation  = stubbing.asTerm
    val transformed = doTransformInvocation(invocation)

    // Build: new org.mockito.IdiomaticMockitoBaseRuntime.ThrowActions[T](Mockito.when[T](transformed))
    val throwActionsClass = Symbol.requiredClass("org.mockito.IdiomaticMockitoBaseRuntime.ThrowActions")
    val mockitoClass      = Symbol.requiredModule("org.mockito.Mockito")

    val whenCall = TypeApply(
      Select.unique(Ref(mockitoClass), "when"),
      List(TypeTree.of[T])
    ).appliedTo(transformed)

    Apply(
      Select.overloaded(New(TypeTree.ref(throwActionsClass)), "<init>", List(TypeRepr.of[T]), Nil),
      List(whenCall)
    ).asExprOf[OngoingStubbing[T]]
  }

  inline def shouldAnswer[T, P1, R](inline stubbing: => T, inline f: Any)(using $pt: Prettifier): AnswerActions[T] =
    ${ shouldAnswerImpl[T, P1, R]('stubbing, 'f, '$pt) }

  def shouldAnswerImpl[T: Type, P1: Type, R: Type](
      stubbing: Expr[T],
      f: Expr[Any],
      pt: Expr[Prettifier]
  )(using Quotes): Expr[AnswerActions[T]] = {
    import quotes.reflect.*

    val invocation  = stubbing.asTerm
    val transformed = doTransformInvocation(invocation)

    '{
      new WhenMacroRuntime.AnswerActions[T](
        org.mockito.Mockito.when[T](${ transformed.asExprOf[T] })
      )
    }
  }

  inline def shouldAnswerPF[T, P, R](inline stubbing: => T, inline f: PartialFunction[P, R])(using $pt: Prettifier): AnswerPFActions[T] =
    ${ shouldAnswerPFImpl[T, P, R]('stubbing, 'f, '$pt) }

  def shouldAnswerPFImpl[T: Type, P: Type, R: Type](
      stubbing: Expr[T],
      f: Expr[PartialFunction[P, R]],
      pt: Expr[Prettifier]
  )(using Quotes): Expr[AnswerPFActions[T]] = {
    import quotes.reflect.*

    val invocation  = stubbing.asTerm
    val transformed = doTransformInvocation(invocation)

    '{
      new WhenMacroRuntime.AnswerPFActions[T](
        org.mockito.Mockito.when[T](${ transformed.asExprOf[T] })
      )
    }
  }
}
