package org.mockito.specs2

import org.mockito.{ Specs2VerifyMacro, VerifyOrder }
import org.specs2.matcher.MatchResultCombinators.combineMatchResult
import org.specs2.matcher.MatchResult

/**
 * Scala 3 compatibility layer for Mockito specs2 integration.
 */
private[specs2] trait MockitoCompat extends MockitoSpecs2Support {
  protected def combineVerifications(left: Verification, right: => Verification): Verification =
    left and right

  /**
   * class supporting 'was' and 'were' methods to forward mockito calls to the CallsMatcher matcher
   */
  class Calls {
    transparent inline def were[T](inline calls: => T)(using inline order: VerifyOrder): Verification =
      Specs2VerifyMacro.wasMacro[T, Verification](calls)
    transparent inline def was[T](inline calls: T)(using inline order: VerifyOrder): Verification =
      Specs2VerifyMacro.wasMacro[T, Verification](calls)
  }

  extension [T](m: MatchResult[T])
    transparent inline def andThen[O](inline calls: => O)(using inline order: VerifyOrder): Verification =
      combineVerifications(m, Specs2VerifyMacro.wasMacro[O, Verification](calls))

  transparent inline def got[T](inline calls: => T)(using inline order: VerifyOrder): Verification =
    Specs2VerifyMacro.gotMacro[T, Verification](calls)
}
