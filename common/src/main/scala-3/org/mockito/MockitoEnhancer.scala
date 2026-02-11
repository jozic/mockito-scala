package org.mockito

import org.mockito.internal.ValueClassExtractor
import org.mockito.stubbing.Stubber
import scala.annotation.targetName

/**
 * Scala 3 specific trait that provides object mocking functionality. Extends MockCreator (Scala 3 version) and MockitoEnhancerRuntime (shared utilities including withObject
 * methods).
 *
 * This is a thin compatibility layer - all actual functionality is in MockCreator and MockitoEnhancerRuntime.
 */
private[mockito] trait MockitoEnhancer extends MockCreator with MockitoEnhancerRuntime {
  // Scala 3 resolves () => R as Unit => R (Function1) instead of by-name.
  // This explicit Function0 overload ensures correct dispatch for doAnswer(() => expr).
  @targetName("doAnswerFunction0")
  def doAnswer[R: ValueClassExtractor](f: () => R): Stubber =
    Mockito.doAnswer(invocationToAnswer[R](_ => f()))
}
