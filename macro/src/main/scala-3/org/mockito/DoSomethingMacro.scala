package org.mockito

import org.mockito.Utils.*
import scala.quoted.*

/**
 * Scala 3 macro implementations for "do something by" DSL (e.g., Returned(value) by mock.method(args))
 */
object DoSomethingMacro {

  /**
   * Generic macro: takes a pre-built Stubber and an invocation, transforms args. Used by cats/scalaz modules to implement returnedF/answeredF/raised etc.
   */
  inline def doSomethingBy[T](inline action: org.mockito.stubbing.Stubber, inline stubbing: T): T =
    ${ doSomethingByImpl[T]('action, 'stubbing) }

  def doSomethingByImpl[T: Type](action: Expr[org.mockito.stubbing.Stubber], stubbing: Expr[T])(using Quotes): Expr[T] = {
    import quotes.reflect.*
    doTransformInvocation(stubbing.asTerm, action.asTerm).asExprOf[T]
  }

  /**
   * Macro for: mock.method(args) doesNothing
   */
  inline def doesNothing[T](inline stubbing: => T): T =
    ${ doesNothingImpl[T]('stubbing) }

  def doesNothingImpl[T: Type](stubbing: Expr[T])(using Quotes): Expr[T] = {
    import quotes.reflect.*

    val invocation = stubbing.asTerm

    // Use quoted expression to call Mockito.doNothing()
    val doNothingCall = '{ org.mockito.Mockito.doNothing() }.asTerm

    doTransformInvocation(invocation, doNothingCall).asExprOf[T]
  }

  /**
   * Macro for: Returned(value) by mock.method(args)
   */
  inline def returnedBy[T, S](inline v: T, inline stubbing: S)(using inline $ev: T <:< S): S =
    ${ returnedByImpl[T, S]('v, 'stubbing) }

  // Wrapper for case class usage
  inline def returnedByMacro[T, S](inline v: T, inline stubbing: S): S =
    ${ returnedByImpl[T, S]('v, 'stubbing) }

  def returnedByImpl[T: Type, S: Type](v: Expr[T], stubbing: Expr[S])(using Quotes): Expr[S] = {
    import quotes.reflect.*

    // Type safety check: the return value type T must be assignable to the stubbed method's return type S
    if !(TypeRepr.of[T] <:< TypeRepr.of[S]) then report.errorAndAbort(s"Type mismatch: value of type ${TypeRepr.of[T].show} is not a subtype of ${TypeRepr.of[S].show}")

    val invocation = stubbing.asTerm

    // Use the Mockito Java API directly by building a quoted expression
    // For value classes, we need to extract the underlying value for Mockito
    // Explicitly type as Stubber to help type inference in transformInvocation
    val stubberType  = TypeRepr.of[org.mockito.stubbing.Stubber]
    val doReturnCall = Typed(
      '{ org.mockito.Mockito.doReturn(org.mockito.internal.ValueClassExtractor[T].extract($v)) }.asTerm,
      TypeTree.of[org.mockito.stubbing.Stubber]
    )

    doTransformInvocation(invocation, doReturnCall).asExprOf[S]
  }

  /**
   * Macro for: Answered(func) by mock.method(args)
   */
  inline def answeredBy[T, S](inline v: T, inline stubbing: S)(using inline $ev: T <:< S): S =
    ${ answeredByImpl[T, S]('v, 'stubbing) }

  // Wrapper for case class usage
  inline def answeredByMacro[T, S](inline v: T, inline stubbing: S): S =
    ${ answeredByImpl[T, S]('v, 'stubbing) }

  // Thunk-based wrapper: value is deferred via () => T to prevent eager evaluation
  inline def answeredByThunkMacro[T, S](inline v: () => T, inline stubbing: S): S =
    ${ answeredByThunkImpl[T, S]('v, 'stubbing) }

  def answeredByThunkImpl[T: Type, S: Type](v: Expr[() => T], stubbing: Expr[S])(using Quotes): Expr[S] = {
    import quotes.reflect.*

    val tRepr = TypeRepr.of[T].dealias.widen
    val sRepr = TypeRepr.of[S].dealias.widen

    val functionInfo: Option[(List[TypeRepr], TypeRepr)] = tRepr match {
      case AppliedType(tycon, args) if args.nonEmpty && tycon.typeSymbol.fullName.startsWith("scala.Function") =>
        Some((args.init, args.last))
      case _ => None
    }
    val resultType = functionInfo.map(_._2).getOrElse(tRepr)
    if !(resultType <:< sRepr) then report.errorAndAbort(s"Type mismatch: answer result type ${resultType.show} is not a subtype of ${sRepr.show}")

    val invocation = stubbing.asTerm

    val doAnswerCall = functionInfo match {
      case Some((paramTypes, retType)) =>
        // Function type: call thunk to get the function, then apply it
        val fnExpr: Expr[T] = '{ $v() }
        val answerExpr      = buildFunctionAnswer(fnExpr, paramTypes, retType)
        '{ org.mockito.Mockito.doAnswer($answerExpr) }.asTerm
      case None =>
        // Plain value: call thunk each invocation for deferred evaluation
        '{ org.mockito.Mockito.doAnswer(org.mockito.stubbing.ScalaAnswer.lift[Any](_ => $v())) }.asTerm
    }

    doTransformInvocation(invocation, doAnswerCall).asExprOf[S]
  }

  def answeredByImpl[T: Type, S: Type](v: Expr[T], stubbing: Expr[S])(using Quotes): Expr[S] = {
    import quotes.reflect.*

    // Type safety check: the function's result type must be compatible with S
    val tRepr = TypeRepr.of[T].dealias.widen
    val sRepr = TypeRepr.of[S].dealias.widen

    val functionInfo: Option[(List[TypeRepr], TypeRepr)] = tRepr match {
      case AppliedType(tycon, args) if args.nonEmpty && tycon.typeSymbol.fullName.startsWith("scala.Function") =>
        Some((args.init, args.last))
      case _ => None
    }
    val resultType = functionInfo.map(_._2).getOrElse(tRepr)
    if !(resultType <:< sRepr) then report.errorAndAbort(s"Type mismatch: answer result type ${resultType.show} is not a subtype of ${sRepr.show}")

    val invocation = stubbing.asTerm

    val doAnswerCall = functionInfo match {
      case Some((paramTypes, retType)) =>
        // Function type: extract args from InvocationOnMock and apply the function
        val answerExpr = buildFunctionAnswer(v, paramTypes, retType)
        '{ org.mockito.Mockito.doAnswer($answerExpr) }.asTerm
      case None =>
        // Plain value: wrap in ScalaAnswer.lift
        '{ org.mockito.Mockito.doAnswer(org.mockito.stubbing.ScalaAnswer.lift[Any](_ => $v)) }.asTerm
    }

    doTransformInvocation(invocation, doAnswerCall).asExprOf[S]
  }

  /** Build a ScalaAnswer that applies a function to extracted InvocationOnMock args with value class support */
  private def buildFunctionAnswer[T: Type](using
      Quotes
  )(
      fn: Expr[T],
      paramTypes: List[quotes.reflect.TypeRepr],
      retType: quotes.reflect.TypeRepr
  ): Expr[org.mockito.stubbing.ScalaAnswer[Any]] = {
    import quotes.reflect.*

    retType.asType match {
      case '[r] =>
        '{
          org.mockito.stubbing.ScalaAnswer.lift[Any] { invocation =>
            ${
              val fnTerm   = fn.asTerm
              val argExprs = paramTypes.zipWithIndex.map { case (pt, i) =>
                pt.asType match {
                  case '[p] =>
                    val idxExpr = Expr(i)
                    '{ org.mockito.internal.ValueClassWrapper[p].wrapAs[p](invocation.getArgument($idxExpr)) }.asTerm
                }
              }
              val result = Select.unique(fnTerm, "apply").appliedToArgs(argExprs)
              '{ org.mockito.internal.ValueClassExtractor[r].extractAs[r](${ result.asExprOf[r] }) }.asExprOf[Any]
            }
          }
        }
    }
  }

  /**
   * Macro for: Thrown(exception) by mock.method(args)
   */
  inline def thrownBy[T](inline v: Throwable, inline stubbing: T)(using inline $ev: Throwable): T =
    ${ thrownByImpl[T]('v, 'stubbing) }

  // Wrapper for case class usage
  inline def thrownByMacro[T, E](inline v: E, inline stubbing: T): T =
    ${ thrownByMacroImpl[T, E]('v, 'stubbing) }

  private def thrownByMacroImpl[T: Type, E: Type](v: Expr[E], stubbing: Expr[T])(using Quotes): Expr[T] = {
    import quotes.reflect.*

    // Type safety check: E must be a Throwable
    if !(TypeRepr.of[E] <:< TypeRepr.of[Throwable]) then report.errorAndAbort(s"Type mismatch: ${TypeRepr.of[E].show} is not a subtype of Throwable")

    thrownByImpl[T]('{ $v.asInstanceOf[Throwable] }, stubbing)
  }

  def thrownByImpl[T: Type](v: Expr[Throwable], stubbing: Expr[T])(using Quotes): Expr[T] = {
    import quotes.reflect.*

    val invocation = stubbing.asTerm

    // Use doAnswer with ScalaThrowsException to avoid Mockito's checked exception validation
    val doThrowCall = '{
      org.mockito.Mockito.doAnswer(new org.mockito.internal.stubbing.answers.ScalaThrowsException($v))
    }.asTerm

    doTransformInvocation(invocation, doThrowCall).asExprOf[T]
  }

  /**
   * Macro for: Called by mock.method(args)
   */
  inline def calledBy[T](inline stubbing: T): T =
    ${ calledByImpl[T]('stubbing) }

  def calledByImpl[T: Type](stubbing: Expr[T])(using Quotes): Expr[T] = {
    import quotes.reflect.*

    val invocation = stubbing.asTerm

    // Use quoted expression to call Mockito.doCallRealMethod()
    val doCallRealMethodCall = '{ org.mockito.Mockito.doCallRealMethod() }.asTerm

    doTransformInvocation(invocation, doCallRealMethodCall).asExprOf[T]
  }

  /**
   * Transform invocation: obj.method(args) => action.when(obj).method(transformedArgs)
   */
  private def transformInvocation(using
      Quotes
  )(
      invocation: quotes.reflect.Term,
      action: quotes.reflect.Term,
      hoisted: scala.collection.mutable.ListBuffer[quotes.reflect.Statement],
      matcherValNames: Set[String] = Set.empty
  ): quotes.reflect.Term = {
    import quotes.reflect.*

    invocation match {
      // Handle blocks with hoisted named args
      case Block(stats, expr) =>
        val detectedMatcherVals         = detectMatcherValDefs(stats)
        val (remainingStats, fixedExpr) = inlineMatcherValDefs(stats, expr, detectedMatcherVals)
        val allMatcherVals              = matcherValNames ++ detectedMatcherVals
        val transformed                 = transformInvocation(fixedExpr, action, hoisted, allMatcherVals)
        if remainingStats.nonEmpty then Block(remainingStats, transformed) else transformed

      // Handle inlined expressions with bindings
      case inlined: Inlined =>
        transformInvocation(inlined.body, action, hoisted, matcherValNames)

      // Match: obj.method(args1)(args2)...
      case Apply(select @ Select(obj, methodName), args) =>
        val transformedArgs = transformArgsForApply(select, args, hoisted, matcherValNames)

        val stubberClass = Symbol.requiredClass("org.mockito.stubbing.Stubber")
        val whenSymbol   = stubberClass.declaredMethod("when").head

        val whenCall = TypeApply(
          Select(action, whenSymbol),
          List(obj.tpe.widen.asType match { case '[t] => TypeTree.of[t] })
        ).appliedTo(obj)

        Apply(
          Select(whenCall, select.symbol),
          transformedArgs
        )

      // Match: obj.method[TypeArgs](args)
      case Apply(TypeApply(select @ Select(obj, methodName), targs), args) =>
        val transformedArgs = transformArgsForApply(TypeApply(select, targs), args, hoisted, matcherValNames)

        val stubberClass = Symbol.requiredClass("org.mockito.stubbing.Stubber")
        val whenSymbol   = stubberClass.declaredMethod("when").head

        val whenCall = TypeApply(
          Select(action, whenSymbol),
          List(obj.tpe.widen.asType match { case '[t] => TypeTree.of[t] })
        ).appliedTo(obj)

        Apply(
          TypeApply(
            Select(whenCall, select.symbol),
            targs
          ),
          transformedArgs
        )

      // Match: obj.method (no args)
      case select @ Select(obj, methodName) =>
        val stubberClass = Symbol.requiredClass("org.mockito.stubbing.Stubber")
        val whenSymbol   = stubberClass.declaredMethod("when").head

        val whenCall = TypeApply(
          Select(action, whenSymbol),
          List(obj.tpe.widen.asType match { case '[t] => TypeTree.of[t] })
        ).appliedTo(obj)

        Select(whenCall, select.symbol)

      // Match: obj.method[TypeArgs] (no args)
      case TypeApply(select @ Select(obj, methodName), targs) =>
        val stubberClass = Symbol.requiredClass("org.mockito.stubbing.Stubber")
        val whenSymbol   = stubberClass.declaredMethod("when").head

        val whenCall = TypeApply(
          Select(action, whenSymbol),
          List(obj.tpe.widen.asType match { case '[t] => TypeTree.of[t] })
        ).appliedTo(obj)

        TypeApply(
          Select(whenCall, select.symbol),
          targs
        )

      // Nested Apply - recurse
      case Apply(fun, args) =>
        val transformedArgs = transformArgsForApply(fun, args, hoisted, matcherValNames)
        Apply(
          transformInvocation(fun, action, hoisted, matcherValNames),
          transformedArgs
        )

      case other =>
        report.errorAndAbort(s"Could not transform do-something invocation: ${other.show}")
    }
  }

  private def doTransformInvocation(using Quotes)(invocation: quotes.reflect.Term, action: quotes.reflect.Term): quotes.reflect.Term = {
    import quotes.reflect.*
    val hoisted     = scala.collection.mutable.ListBuffer.empty[Statement]
    val transformed = transformInvocation(invocation, action, hoisted)
    if hoisted.nonEmpty then Block(hoisted.toList, transformed) else transformed
  }
}
