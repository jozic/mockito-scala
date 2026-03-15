package org.mockito

import org.mockito.Utils.*
import org.mockito.verification.VerificationMode
import org.scalactic.Prettifier
import scala.quoted.*

/**
 * Scala 3 macro implementations for the prefix expectations DSL (e.g., expect a call to mock.method(args))
 */
object ExpectMacro {

  inline def callsTo[R](inline stubbedMethodCall: Any, mode: ScalaVerificationMode)(inline order: VerifyOrder): R =
    ${ callsToImpl[R]('stubbedMethodCall, 'mode, 'order) }

  def callsToImpl[R: Type](stubbedMethodCall: Expr[Any], mode: Expr[ScalaVerificationMode], order: Expr[VerifyOrder])(using Quotes): Expr[R] = {
    import quotes.reflect.*

    val application = stubbedMethodCall.asTerm

    // Unwrap Inlined wrappers to get the real expression
    def unwrap(t: Term): Term = t match {
      case Inlined(_, _, body) => unwrap(body)
      case Block(Nil, expr)    => unwrap(expr)
      case other               => other
    }

    val unwrapped = unwrap(application)

    // Detect `fixture.org` pattern (Select with no Apply) — user passed a mock object, not a method call
    unwrapped match {
      case Select(obj, fieldName) if !unwrapped.symbol.isDefDef =>
        // May be a mock object reference; add a runtime guard
        val exprStr = Expr(stubbedMethodCall.show)
        val objExpr = obj.asExprOf[Any]
        val guard   = '{
          if !org.mockito.Mockito.mockingDetails($objExpr.asInstanceOf[AnyRef]).isMock then
            throw new org.mockito.exceptions.misusing.MissingMethodInvocationException(
              s"'expect no calls to <?>' requires an argument which is 'a method call on a mock',\n" +
                s"  but looks like [${$exprStr}] is not a method call on a mock. Is it a mock object?\n\n" +
                "The following would be correct (note the usage of 'calls to' vs 'calls on'):\n" +
                "    expect no calls to aMock.bar(*)\n" +
                "    expect no calls on aMock\n"
            )
        }
        val verifyCall = transformExpectation(application, order, mode)
        Block(List(guard.asTerm), verifyCall).asExprOf[R]
      case _ =>
        transformExpectation(application, order, mode).asExprOf[R]
    }
  }

  inline def callsOn[R](inline mock: Any): R =
    ${ callsOnImpl[R]('mock, '{ false }, '{ false }) }

  inline def callsOnNoMore[R](inline mock: Any, inline ignoringStubs: Boolean): R =
    ${ callsOnImpl[R]('mock, '{ true }, 'ignoringStubs) }

  def callsOnImpl[R: Type](mock: Expr[Any], noMore: Expr[Boolean], ignoringStubs: Expr[Boolean])(using Quotes): Expr[R] = {
    import quotes.reflect.*

    val mockTerm = mock.asTerm

    val isIgnoring = ignoringStubs.value.getOrElse(false)
    val isNoMore   = noMore.value.getOrElse(false)

    (isNoMore, isIgnoring) match {
      case (true, true) =>
        transformNoMoreInteractionsIgnoringStubsExpectation(mockTerm).asExprOf[R]
      case (true, _) =>
        transformNoMoreInteractionsExpectation(mockTerm).asExprOf[R]
      case _ =>
        transformNoInteractionsExpectation(mockTerm).asExprOf[R]
    }
  }

  /** Generates runtime-branching code for noMore calls, for when ignoringStubs is a runtime value */
  def callsOnNoMoreImpl[R: Type](mock: Expr[Any], ignoringStubs: Expr[Boolean])(using Quotes): Expr[R] = {
    import quotes.reflect.*

    // If we can resolve at compile time, do so
    ignoringStubs.value match {
      case Some(true) =>
        transformNoMoreInteractionsIgnoringStubsExpectation(mock.asTerm).asExprOf[R]
      case Some(false) =>
        transformNoMoreInteractionsExpectation(mock.asTerm).asExprOf[R]
      case None =>
        // Runtime value — emit both paths wrapped in if/else
        val noMoreExpr   = transformNoMoreInteractionsExpectation(mock.asTerm)
        val ignoringExpr = transformNoMoreInteractionsIgnoringStubsExpectation(mock.asTerm)
        val branch       = If(ignoringStubs.asTerm, ignoringExpr, noMoreExpr)
        branch.asExprOf[R]
    }
  }

  /** Macro for noMore(calls) / noMore(calls(ignoringStubs)) — constructs the class with the flag */
  def noMoreMacro[R: Type](using Quotes)(callsExpr: Expr[Any]): Expr[R] = {
    import quotes.reflect.*
    val isIgnoring = detectIgnoringStubsInCalls(callsExpr.asTerm)
    val boolLit    = Literal(BooleanConstant(isIgnoring))
    val cls        = TypeRepr.of[R].typeSymbol
    Apply(Select(New(TypeTree.of[R]), cls.primaryConstructor), List(boolLit)).asExprOf[R]
  }

  /** Detect if a CallsWord expression was obtained via calls(ignoringStubs) */
  def detectIgnoringStubsMacro(using Quotes)(callsExpr: Expr[Any]): Expr[Boolean] = {
    import quotes.reflect.*
    Expr(detectIgnoringStubsInCalls(callsExpr.asTerm))
  }

  private def detectIgnoringStubsInCalls(using Quotes)(term: quotes.reflect.Term): Boolean = {
    import quotes.reflect.*
    term match {
      case Inlined(_, _, body) => detectIgnoringStubsInCalls(body)
      case Block(_, expr)      => detectIgnoringStubsInCalls(expr)
      case Apply(_, args)      =>
        // calls.apply(ignoringStubs)
        args.exists { a =>
          val shown = a.show
          shown.contains("ignoringStubs") || shown.contains("IgnoringStubs")
        }
      case _ => false
    }
  }

  private def transformExpectation(using
      Quotes
  )(
      invocation: quotes.reflect.Term,
      order: Expr[VerifyOrder],
      mode: Expr[ScalaVerificationMode]
  ): quotes.reflect.Term = {
    import quotes.reflect.*

    // The invocation is the method call we need to verify
    // For "expect a call to mock.method(args)", invocation is mock.method(args)

    // Build: verification(order.verifyWithMode(mock, mode).method(transformedArgs))
    val hoisted    = scala.collection.mutable.ListBuffer.empty[quotes.reflect.Statement]
    val verifyCall = transformInvocationForExpect(invocation, order.asTerm, mode.asTerm, hoisted)

    // Wrap in verification() call
    val verifyExpr = findVerificationSymbol match {
      case Some((owner, verificationMethod)) =>
        if owner.isClassDef || owner.isType then {
          Apply(
            Select(This(owner), verificationMethod),
            List(verifyCall)
          )
        } else {
          Apply(
            Ref(verificationMethod),
            List(verifyCall)
          )
        }
      case None =>
        report.errorAndAbort("Could not find verification method in scope")
    }

    if hoisted.nonEmpty then Block(hoisted.toList, verifyExpr) else verifyExpr
  }

  private def transformInvocationForExpect(using
      Quotes
  )(
      invocation: quotes.reflect.Term,
      order: quotes.reflect.Term,
      times: quotes.reflect.Term,
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
        val transformed                 = transformInvocationForExpect(fixedExpr, order, times, hoisted, allMatcherVals)
        if remainingStats.nonEmpty then Block(remainingStats, transformed) else transformed

      // Handle inlined expressions
      case inlined: Inlined =>
        transformInvocationForExpect(inlined.body, order, times, hoisted, matcherValNames)

      // Match: obj.method(args1)(args2)...
      case Apply(select @ Select(obj, methodName), args) =>
        val transformedArgs = transformArgsForApply(select, args, hoisted, matcherValNames)

        val objType     = obj.tpe.widen.asType
        val verifiedObj = Apply(
          TypeApply(
            Select.unique(order, "verifyWithMode"),
            List(TypeTree.of(using objType))
          ),
          List(obj, times)
        )

        Apply(
          Select(verifiedObj, select.symbol),
          transformedArgs
        )

      // Match: obj.method[TypeArgs](args)
      case Apply(TypeApply(select @ Select(obj, methodName), targs), args) =>
        val transformedArgs = transformArgsForApply(TypeApply(select, targs), args, hoisted, matcherValNames)

        val objType     = obj.tpe.widen.asType
        val verifiedObj = Apply(
          TypeApply(
            Select.unique(order, "verifyWithMode"),
            List(TypeTree.of(using objType))
          ),
          List(obj, times)
        )

        Apply(
          TypeApply(
            Select(verifiedObj, select.symbol),
            targs
          ),
          transformedArgs
        )

      // Match: obj.method (no args)
      case select @ Select(obj, methodName) =>
        val objType     = obj.tpe.widen.asType
        val verifiedObj = Apply(
          TypeApply(
            Select.unique(order, "verifyWithMode"),
            List(TypeTree.of(using objType))
          ),
          List(obj, times)
        )

        Select(verifiedObj, select.symbol)

      // Match: obj.method[TypeArgs] (no args)
      case TypeApply(select @ Select(obj, methodName), targs) =>
        val objType     = obj.tpe.widen.asType
        val verifiedObj = Apply(
          TypeApply(
            Select.unique(order, "verifyWithMode"),
            List(TypeTree.of(using objType))
          ),
          List(obj, times)
        )

        TypeApply(
          Select(verifiedObj, select.symbol),
          targs
        )

      // Nested Apply - recurse
      case Apply(fun, args) =>
        val transformedArgs = transformArgsForApply(fun, args, hoisted, matcherValNames)
        Apply(
          transformInvocationForExpect(fun, order, times, hoisted, matcherValNames),
          transformedArgs
        )

      case other =>
        report.errorAndAbort(s"Could not transform expect invocation: ${other.show}")
    }
  }

  private def transformNoInteractionsExpectation(using
      Quotes
  )(
      mock: quotes.reflect.Term
  ): quotes.reflect.Term = {
    import quotes.reflect.*

    // For "expect no calls on mock"
    // Build: verification(MockitoSugar.verifyZeroInteractions(mock))
    val mockitoClass           = Symbol.requiredModule("org.mockito.MockitoSugar")
    val verifyZeroInteractions = Select.unique(Ref(mockitoClass), "verifyZeroInteractions")

    val verificationCall = Apply(verifyZeroInteractions, List(mock))

    wrapInVerification(verificationCall)
  }

  private def transformNoMoreInteractionsExpectation(using
      Quotes
  )(
      mock: quotes.reflect.Term
  ): quotes.reflect.Term = {
    import quotes.reflect.*

    // For "expect noMore calls on mock"
    // Build: verification(MockitoSugar.verifyNoMoreInteractions(mock))
    val mockitoClass             = Symbol.requiredModule("org.mockito.MockitoSugar")
    val verifyNoMoreInteractions = Select.unique(Ref(mockitoClass), "verifyNoMoreInteractions")

    val verificationCall = Apply(verifyNoMoreInteractions, List(mock))

    wrapInVerification(verificationCall)
  }

  private def transformNoMoreInteractionsIgnoringStubsExpectation(using
      Quotes
  )(
      mock: quotes.reflect.Term
  ): quotes.reflect.Term = {
    import quotes.reflect.*

    // For "expect noMore calls(ignoringStubs) on mock"
    // ignoreStubs marks stubbed invocations as ignored, then verifyNoMoreInteractions checks the rest
    // Build: verification { ignoreStubs(mock); verifyNoMoreInteractions(mock) }
    val mockitoClass    = Symbol.requiredModule("org.mockito.MockitoSugar")
    val ignoreStubsCall = Apply(Select.unique(Ref(mockitoClass), "ignoreStubs"), List(mock))
    val verifyCall      = Apply(Select.unique(Ref(mockitoClass), "verifyNoMoreInteractions"), List(mock))
    val block           = Block(List(ignoreStubsCall), verifyCall)

    wrapInVerification(block)
  }

  private def wrapInVerification(using Quotes)(call: quotes.reflect.Term): quotes.reflect.Term = {
    import quotes.reflect.*

    // Wrap in verification() call by finding it in the owner chain
    findVerificationSymbol match {
      case Some((owner, verificationMethod)) =>
        if owner.isClassDef || owner.isType then {
          Apply(
            Select(This(owner), verificationMethod),
            List(call)
          )
        } else {
          Apply(
            Ref(verificationMethod),
            List(call)
          )
        }
      case None =>
        report.errorAndAbort("Could not find verification method in scope")
    }
  }

  /**
   * Find the verification method in the owner chain
   */
  private def findVerificationSymbol(using Quotes): Option[(quotes.reflect.Symbol, quotes.reflect.Symbol)] = {
    import quotes.reflect.*

    var current = Symbol.spliceOwner

    // Walk up the owner chain
    while current != Symbol.noSymbol && current != defn.RootClass do {
      // Try to find verification method (including inherited methods)
      try {
        val methods = current.methodMembers.filter(_.name == "verification")
        if methods.nonEmpty then {
          return Some((current, methods.head))
        }
      } catch {
        case _: Exception =>
        // Skip this level and continue up the chain
      }

      current = current.owner
    }

    None
  }
}
