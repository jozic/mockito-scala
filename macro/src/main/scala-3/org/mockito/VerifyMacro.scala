package org.mockito

import org.mockito.Utils.*
import org.mockito.VerifyMacroRuntime.{ Never, NeverAgain, Once }
import org.scalactic.Prettifier

import scala.quoted.*

/**
 * Scala 3 macro implementations for the idiomatic verification DSL. Runtime support classes are in VerifyMacroRuntime.
 */
object Called {
  inline def by[T](inline stubbing: T): T = ${ DoSomethingMacro.calledByImpl[T]('stubbing) }
}

object VerifyMacro {
  // Re-export runtime objects for backward compatibility
  val Never      = VerifyMacroRuntime.Never
  val NeverAgain = VerifyMacroRuntime.NeverAgain
  val Once       = VerifyMacroRuntime.Once

  /**
   * Macro for: mock.method(args) was called
   */
  inline def wasMacro[T, R](inline stubbing: T, inline called: Any)(using inline order: VerifyOrder): R =
    ${ wasMacroImpl[T, R]('stubbing, 'called, 'order) }

  def wasMacroImpl[T: Type, R: Type](
      stubbing: Expr[T],
      called: Expr[Any],
      order: Expr[VerifyOrder]
  )(using Quotes): Expr[R] = {
    import quotes.reflect.*

    val invocation = stubbing.asTerm
    val times      = '{ VerifyMacroRuntime.Once }

    transformVerification(invocation, order.asTerm, times.asTerm).asExprOf[R]
  }

  /**
   * Macro for: mock.method(args) wasNever called OR mock wasNever called
   */
  inline def wasNeverMacro[T, R](inline stubbing: T, inline called: Any)(using inline order: VerifyOrder): R =
    ${ wasNeverMacroImpl[T, R]('stubbing, 'called, 'order) }

  def wasNeverMacroImpl[T: Type, R: Type](
      stubbing: Expr[T],
      called: Expr[Any],
      order: Expr[VerifyOrder]
  )(using Quotes): Expr[R] = {
    import quotes.reflect.*

    val invocation = stubbing.asTerm

    // Check if this is verifying a mock object (no method call) vs a method invocation
    if isMethodInvocation(invocation) then {
      // Method call: mock.method() wasNever called
      val times = '{ VerifyMacroRuntime.Never }
      transformVerification(invocation, order.asTerm, times.asTerm).asExprOf[R]
    } else {
      // Just a mock object: mock wasNever called
      // Build: verification(MockitoSugar.verifyZeroInteractions(obj))
      val mockitoSugar = Symbol.requiredModule("org.mockito.MockitoSugar")

      val verifyCall = Apply(
        Select.unique(Ref(mockitoSugar), "verifyZeroInteractions"),
        List(invocation)
      )

      // Build: verification(verifyCall)
      findVerificationSymbol match {
        case Some((owner, verificationMethod)) =>
          if owner.isClassDef || owner.isType then {
            Apply(
              Select(This(owner), verificationMethod),
              List(verifyCall)
            ).asExprOf[R]
          } else {
            Apply(
              Ref(verificationMethod),
              List(verifyCall)
            ).asExprOf[R]
          }
        case None =>
          report.errorAndAbort(s"Could not find 'verification' method in scope. Searched from: ${Symbol.spliceOwner.fullName}")
      }
    }
  }

  /**
   * Macro for: mock.method(args) wasCalled times(n)
   */
  inline def wasCalledMacro[T, R](inline stubbing: T, inline times: ScalaVerificationMode)(using inline order: VerifyOrder): R =
    ${ wasCalledMacroImpl[T, R]('stubbing, 'times, 'order) }

  def wasCalledMacroImpl[T: Type, R: Type](
      stubbing: Expr[T],
      times: Expr[ScalaVerificationMode],
      order: Expr[VerifyOrder]
  )(using Quotes): Expr[R] = {
    import quotes.reflect.*

    val invocation = stubbing.asTerm

    transformVerification(invocation, order.asTerm, times.asTerm).asExprOf[R]
  }

  /**
   * Macro for: mock wasNever calledAgain
   */
  inline def wasNeverCalledAgainMacro[T, R](inline stubbing: T, inline called: Any): R =
    ${ wasNeverCalledAgainMacroImpl[T, R]('stubbing, 'called) }

  def wasNeverCalledAgainMacroImpl[T: Type, R: Type](
      stubbing: Expr[T],
      called: Expr[Any]
  )(using Quotes): Expr[R] = {
    import quotes.reflect.*

    val obj          = stubbing.asTerm
    val mockitoSugar = Symbol.requiredModule("org.mockito.MockitoSugar")
    val exprStr      = Expr(stubbing.show)

    // Detect if called is LenientCalledAgain (i.e. calledAgain(ignoringStubs))
    val isIgnoringStubs = detectIgnoringStubs(called.asTerm)

    // Wrap in try-catch to provide better error messages
    val tryBody: Expr[Unit] = if isIgnoringStubs then {
      '{ org.mockito.Mockito.ignoreStubs($stubbing.asInstanceOf[AnyRef]); org.mockito.Mockito.verifyNoMoreInteractions($stubbing.asInstanceOf[AnyRef]) }
    } else {
      '{ org.mockito.Mockito.verifyNoMoreInteractions($stubbing.asInstanceOf[AnyRef]) }
    }

    val wrappedCall = '{
      try $tryBody
      catch {
        case _: org.mockito.exceptions.misusing.NotAMockException =>
          throw new org.mockito.exceptions.misusing.NotAMockException(
            s"[${$exprStr}] is not a mock!\nExample of correct verification:\n    myMock wasNever called\n"
          )
      }
    }

    // Build: verification(wrappedCall)
    findVerificationSymbol match {
      case Some((owner, verificationMethod)) =>
        if owner.isClassDef || owner.isType then {
          Apply(
            Select(This(owner), verificationMethod),
            List(wrappedCall.asTerm)
          ).asExprOf[R]
        } else {
          Apply(
            Ref(verificationMethod),
            List(wrappedCall.asTerm)
          ).asExprOf[R]
        }
      case None =>
        report.errorAndAbort(s"Could not find 'verification' method in scope. Searched from: ${Symbol.spliceOwner.fullName}")
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
      // The try-catch handles CyclicReference errors that can occur in some contexts
      try {
        val methods = current.methodMembers.filter(_.name == "verification")
        if methods.nonEmpty then {
          return Some((current, methods.head))
        }
      } catch {
        case _: Exception =>
        // Skip this level and continue up the chain (handles CyclicReference and other issues)
      }

      current = current.owner
    }

    None
  }

  /**
   * Transform a method invocation into a verification call
   */
  private def transformVerification(using
      Quotes
  )(
      invocation: quotes.reflect.Term,
      order: quotes.reflect.Term,
      times: quotes.reflect.Term
  ): quotes.reflect.Term = {
    import quotes.reflect.*

    val hoisted     = scala.collection.mutable.ListBuffer.empty[Statement]
    val transformed = transformInvocationForVerify(invocation, order, times, hoisted)

    // Build: verification(transformed)
    val verifyExpr = findVerificationSymbol match {
      case Some((owner, verificationMethod)) =>
        if owner.isClassDef || owner.isType then {
          Apply(
            Select(This(owner), verificationMethod),
            List(transformed)
          )
        } else {
          Apply(
            Ref(verificationMethod),
            List(transformed)
          )
        }
      case None =>
        report.errorAndAbort(s"Could not find 'verification' method in scope. Searched from: ${Symbol.spliceOwner.fullName}")
    }

    if hoisted.nonEmpty then Block(hoisted.toList, verifyExpr) else verifyExpr
  }

  /**
   * Check if a term is a method invocation (vs just an identifier/object/field access). A method invocation can be:
   *   - Apply(...) - method with arguments
   *   - Select(obj, methodName) where methodName is a def (parameterless method)
   *   - Ident(name) where name is a def
   */
  private def isMethodInvocation(using Quotes)(term: quotes.reflect.Term): Boolean = {
    import quotes.reflect.*

    term match {
      case Apply(_, _)         => true
      case TypeApply(fun, _)   => isMethodInvocation(fun) // Check if the function part is a method call
      case Block(_, last)      => isMethodInvocation(last)
      case Inlined(_, _, body) => isMethodInvocation(body)
      case select: Select      =>
        // A Select is a method call if the selected symbol is a method (def)
        select.symbol.isDefDef
      case ident: Ident =>
        // An Ident is a method call if it refers to a method
        ident.symbol.isDefDef
      case _ => false
    }
  }

  /**
   * Inline bindings by substituting all references to bound variables with their RHS values. This avoids type inference issues that can occur when transformed code references vals
   * from bindings.
   */
  private def inlineBindings(using
      Quotes
  )(
      expr: quotes.reflect.Term,
      bindings: List[quotes.reflect.Definition]
  ): quotes.reflect.Term = {
    import quotes.reflect.*

    if bindings.isEmpty then return expr

    // Build a map of variable names to their RHS values
    val substitutions = bindings.collect {
      case valDef: ValDef if valDef.rhs.isDefined =>
        valDef.name -> valDef.rhs.get
    }.toMap

    // Use TreeMap to substitute all Ident references
    val transformer = new TreeMap {
      override def transformTerm(tree: Term)(owner: Symbol): Term = tree match {
        case Ident(name) if substitutions.contains(name) =>
          // Replace the identifier with its value
          substitutions(name)
        case _ =>
          super.transformTerm(tree)(owner)
      }
    }

    transformer.transformTerm(expr)(Symbol.spliceOwner)
  }

  /**
   * Transform invocation: obj.method(args) => order.verifyWithMode(obj, times).method(transformedArgs)
   *
   * This handles the complex case where inline parameters create Inlined nodes with bindings (ValDefs) that define proxy values referenced in the expansion.
   */
  private def transformInvocationForVerify(using
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
        val transformed                 = transformInvocationForVerify(fixedExpr, order, times, hoisted, allMatcherVals)
        if remainingStats.nonEmpty then Block(remainingStats, transformed) else transformed

      // Handle inlined expressions with bindings
      case Inlined(call, bindings, expansion) =>
        transformInvocationForVerify(expansion, order, times, hoisted, matcherValNames)

      case _ => transformMethodCall(invocation, order, times, hoisted, matcherValNames)
    }
  }

  /**
   * Transform a method call for verification
   */
  private def transformMethodCall(using
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
      // Match: obj.method(args1)(args2)...
      case Apply(select @ Select(obj, methodName), args) =>
        val transformedArgs = transformArgsForApply(select, args, hoisted, matcherValNames)

        // Build: order.verifyWithMode[T](obj, times).method(transformedArgs)
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

        // Build: order.verifyWithMode[T](obj, times).method[TypeArgs](transformedArgs)
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
        // Build: order.verifyWithMode[T](obj, times).method
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
        // Build: order.verifyWithMode[T](obj, times).method[TypeArgs]
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

      // Nested Apply - recurse (handles multiple param lists)
      case Apply(fun, args) =>
        val transformedArgs = transformArgsForApply(fun, args, hoisted, matcherValNames)
        Apply(
          transformMethodCall(fun, order, times, hoisted, matcherValNames),
          transformedArgs
        )

      case other =>
        report.errorAndAbort(s"Could not transform verification invocation: ${other.show}")
    }
  }

  /** Detect if a CalledAgain expression is LenientCalledAgain (i.e. calledAgain(ignoringStubs)) by inspecting the AST. */
  private def detectIgnoringStubs(using Quotes)(term: quotes.reflect.Term): Boolean = {
    import quotes.reflect.*
    term match {
      case Inlined(_, _, body)                                                                        => detectIgnoringStubs(body)
      case Block(_, expr)                                                                             => detectIgnoringStubs(expr)
      case sel: Select if sel.symbol.fullName.contains("LenientCalledAgain")                          => true
      case ident: Ident if ident.symbol.fullName.contains("LenientCalledAgain")                       => true
      case Apply(fun, _) if fun.symbol.fullName.contains("CalledAgain") && fun.symbol.name == "apply" => true
      case _                                                                                          => false
    }
  }
}
