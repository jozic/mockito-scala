package org.mockito

import org.mockito.MacroConstants.WordsToNumbers
import org.mockito.Utils.transformArgsForApply

import scala.quoted.*

object Specs2VerifyMacro {
  private case class Times(times: Int) extends ScalaVerificationMode {
    override def verificationMode: org.mockito.verification.VerificationMode = Mockito.times(times)
  }
  private case class AtLeast(times: Int) extends ScalaVerificationMode {
    override def verificationMode: org.mockito.verification.VerificationMode = Mockito.atLeast(times)
  }
  private case class AtMost(times: Int) extends ScalaVerificationMode {
    override def verificationMode: org.mockito.verification.VerificationMode = Mockito.atMost(times)
  }

  inline def wasMacro[T, R](inline calls: T)(using inline order: VerifyOrder): R =
    ${ wasMacroImpl[T, R]('calls, 'order) }

  inline def gotMacro[T, R](inline calls: T)(using inline order: VerifyOrder): R =
    ${ gotMacroImpl[T, R]('calls, 'order) }

  private def wasMacroImpl[T: Type, R: Type](calls: Expr[T], order: Expr[VerifyOrder])(using Quotes): Expr[R] =
    import quotes.reflect.*
    transformMaybeVerification(calls.asTerm, order.asTerm).asExprOf[R]

  private def gotMacroImpl[T: Type, R: Type](calls: Expr[T], order: Expr[VerifyOrder])(using Quotes): Expr[R] = {
    import quotes.reflect.*

    def collectTerms(term: Term): List[Term] = term match {
      case Inlined(_, _, body) => collectTerms(body)
      case Block(stats, expr)  => stats.collect { case t: Term => t } ++ collectTerms(expr)
      case other               => List(other)
    }

    val terms   = collectTerms(calls.asTerm)
    val first   = transformMaybeVerification(terms.head, order.asTerm)
    val chained = terms.tail.foldLeft(first) { (acc, next) =>
      combineVerifications(acc, transformMaybeVerification(next, order.asTerm))
    }
    chained.asExprOf[R]
  }

  private def transformMaybeVerification(using Quotes)(term: quotes.reflect.Term, order: quotes.reflect.Term): quotes.reflect.Term =
    if isAlreadyVerification(term) then term
    else transformSpecs2Verification(term, order).getOrElse(term)

  private def transformSpecs2Verification(using
      Quotes
  )(
      term: quotes.reflect.Term,
      order: quotes.reflect.Term
  ): Option[quotes.reflect.Term] = {
    import quotes.reflect.*

    def strip(t: Term): Term = t match {
      case Inlined(_, _, body) => strip(body)
      case Block(Nil, expr)    => strip(expr)
      case other               => other
    }

    def buildMode(name: String, times: Term): Term =
      name.toLowerCase match {
        case "exactly" => '{ Times(${ times.asExprOf[Int] }) }.asTerm
        case "atleast" => '{ AtLeast(${ times.asExprOf[Int] }) }.asTerm
        case "atmost"  => '{ AtMost(${ times.asExprOf[Int] }) }.asTerm
      }

    def extractModeAndMock(t: Term): Option[(Term, Term)] = strip(t) match {
      case Inlined(_, _, body) =>
        extractModeAndMock(body)

      case Block(_, expr) =>
        extractModeAndMock(expr)

      case Typed(expr, _) =>
        extractModeAndMock(expr)

      case Apply(TypeApply(Select(_, method), _), List(obj)) if WordsToNumbers.contains(method) =>
        Some((obj, buildMode("exactly", Literal(IntConstant(WordsToNumbers(method))))))

      case Apply(Select(_, method), List(obj)) if WordsToNumbers.contains(method) =>
        Some((obj, buildMode("exactly", Literal(IntConstant(WordsToNumbers(method))))))

      case Apply(Apply(TypeApply(Select(_, method), _), List(times)), List(obj)) if Set("exactly", "atLeast", "atMost").contains(method) =>
        Some((obj, buildMode(method, times)))

      case Apply(Apply(Select(_, method), List(times)), List(obj)) if Set("exactly", "atLeast", "atMost").contains(method) =>
        Some((obj, buildMode(method, times)))

      case Apply(TypeApply(Select(Apply(TypeApply(Select(_, "Specs2IntOps"), _), List(times)), "times"), _), List(obj)) =>
        Some((obj, buildMode("exactly", times)))

      case Apply(Select(Apply(TypeApply(Select(_, "Specs2IntOps"), _), List(times)), "times"), List(obj)) =>
        Some((obj, buildMode("exactly", times)))

      case Apply(fun, _) =>
        extractModeAndMock(fun)

      case TypeApply(fun, _) =>
        extractModeAndMock(fun)

      case _ => None
    }

    def verificationCall(v: Term): Term =
      findVerificationSymbol match {
        case Some((owner, verificationMethod)) =>
          if owner.isClassDef || owner.isType then Apply(Select(This(owner), verificationMethod), List(v))
          else Apply(Ref(verificationMethod), List(v))
        case None =>
          report.errorAndAbort(s"Could not find 'verification' method in scope. Searched from: ${Symbol.spliceOwner.fullName}")
      }

    def zeroInteractions(obj: Term): Term = {
      val mockitoSugar = Symbol.requiredModule("org.mockito.MockitoSugar")
      verificationCall(Apply(Select.unique(Ref(mockitoSugar), "verifyZeroInteractions"), List(obj)))
    }

    def noMoreInteractions(obj: Term): Term = {
      val mockitoSugar = Symbol.requiredModule("org.mockito.MockitoSugar")
      verificationCall(Apply(Select.unique(Ref(mockitoSugar), "verifyNoMoreInteractions"), List(obj)))
    }

    def verifiedWith(orderTerm: Term, obj: Term, mode: Term): Term = {
      val objType = obj.tpe.widen.asType
      Apply(TypeApply(Select.unique(orderTerm, "verifyWithMode"), List(TypeTree.of(using objType))), List(obj, mode))
    }

    def transformMethodCall(invocation: Term, hoisted: scala.collection.mutable.ListBuffer[Statement]): Term = invocation match {
      case Apply(select @ Select(objExpr, _), args) =>
        extractModeAndMock(objExpr) match {
          case Some((obj, mode)) =>
            val transformedArgs = transformArgsForApply(select, args, hoisted)
            Apply(Select(verifiedWith(order, obj, mode), select.symbol), transformedArgs)
          case None =>
            val transformedArgs = transformArgsForApply(select, args, hoisted)
            Apply(transformMethodCall(select, hoisted), transformedArgs)
        }

      case Apply(TypeApply(select @ Select(objExpr, _), targs), args) =>
        extractModeAndMock(objExpr) match {
          case Some((obj, mode)) =>
            val transformedArgs = transformArgsForApply(TypeApply(select, targs), args, hoisted)
            Apply(TypeApply(Select(verifiedWith(order, obj, mode), select.symbol), targs), transformedArgs)
          case None =>
            val transformedArgs = transformArgsForApply(TypeApply(select, targs), args, hoisted)
            Apply(transformMethodCall(TypeApply(select, targs), hoisted), transformedArgs)
        }

      case select @ Select(objExpr, _) =>
        extractModeAndMock(objExpr)
          .map { case (obj, mode) =>
            Select(verifiedWith(order, obj, mode), select.symbol)
          }
          .getOrElse(select)

      case TypeApply(select @ Select(objExpr, _), targs) =>
        extractModeAndMock(objExpr)
          .map { case (obj, mode) =>
            TypeApply(Select(verifiedWith(order, obj, mode), select.symbol), targs)
          }
          .getOrElse(TypeApply(select, targs))

      case Apply(fun, args) =>
        val transformedArgs = transformArgsForApply(fun, args, hoisted)
        Apply(transformMethodCall(fun, hoisted), transformedArgs)

      case Inlined(_, _, body) =>
        transformMethodCall(body, hoisted)

      case Block(stats, expr) =>
        val transformed = transformMethodCall(expr, hoisted)
        if stats.nonEmpty then Block(stats, transformed) else transformed

      case other =>
        other
    }

    strip(term) match {
      case Apply(TypeApply(Select(_, "noCallsTo"), _), List(obj)) =>
        Some(zeroInteractions(obj))
      case Apply(Select(_, "noCallsTo"), List(obj)) =>
        Some(zeroInteractions(obj))
      case Apply(TypeApply(Select(_, "noMoreCallsTo"), _), List(obj)) =>
        Some(noMoreInteractions(obj))
      case Apply(Select(_, "noMoreCallsTo"), List(obj)) =>
        Some(noMoreInteractions(obj))
      case invocation =>
        val hoisted     = scala.collection.mutable.ListBuffer.empty[Statement]
        val transformed = transformMethodCall(invocation, hoisted)
        if transformed == invocation && !containsSpecs2Dsl(invocation) then None
        else Some(if hoisted.nonEmpty then Block(hoisted.toList, verificationCall(transformed)) else verificationCall(transformed))
    }
  }

  private def isAlreadyVerification(using Quotes)(term: quotes.reflect.Term): Boolean = {
    import quotes.reflect.*
    term match {
      case Inlined(_, _, body)                                 => isAlreadyVerification(body)
      case Block(_, expr)                                      => isAlreadyVerification(expr)
      case Apply(Select(_, name), _) if name == "verification" => true
      case Apply(Ident(name), _) if name == "verification"     => true
      case _                                                   => false
    }
  }

  private def containsSpecs2Dsl(using Quotes)(term: quotes.reflect.Term): Boolean = {
    import quotes.reflect.*

    def loop(t: Term): Boolean = t match {
      case Inlined(_, _, body) => loop(body)
      case Block(stats, expr)  => stats.collect { case tt: Term => tt }.exists(loop) || loop(expr)
      case Apply(fun, args)    => loop(fun) || args.exists(loop)
      case TypeApply(fun, _)   => loop(fun)
      case Select(qual, name)  =>
        Set("one", "two", "three", "no", "exactly", "atLeast", "atMost", "times", "noCallsTo", "noMoreCallsTo").contains(name) || loop(qual)
      case _ => false
    }

    loop(term)
  }

  private def findVerificationSymbol(using Quotes): Option[(quotes.reflect.Symbol, quotes.reflect.Symbol)] = {
    import quotes.reflect.*

    var current = Symbol.spliceOwner
    while current != Symbol.noSymbol && current != defn.RootClass do {
      try {
        val methods = current.methodMembers.filter(_.name == "verification")
        if methods.nonEmpty then return Some((current, methods.head))
      } catch {
        case _: Exception =>
      }
      current = current.owner
    }
    None
  }

  private def combineVerifications(using Quotes)(lhs: quotes.reflect.Term, rhs: quotes.reflect.Term): quotes.reflect.Term = {
    import quotes.reflect.*

    var current = Symbol.spliceOwner
    while current != Symbol.noSymbol && current != defn.RootClass do {
      try {
        val methods = current.methodMembers.filter(_.name == "combineVerifications")
        if methods.nonEmpty then {
          val method = methods.head
          return if current.isClassDef || current.isType then Apply(Select(This(current), method), List(lhs, rhs))
          else Apply(Ref(method), List(lhs, rhs))
        }
      } catch {
        case _: Exception =>
      }
      current = current.owner
    }
    report.errorAndAbort(s"Could not find 'combineVerifications' method in scope. Searched from: ${Symbol.spliceOwner.fullName}")
  }

}
