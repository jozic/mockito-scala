package org.mockito.scalatest

import org.mockito.invocation.MockHandler
import org.mockito.mock.MockCreationSettings
import org.mockito.{ MockCreatorRuntime, MockSettings }
import org.scalactic.Prettifier

import scala.reflect.ClassTag

/**
 * Scala 3 compatibility layer for `ResetMocksAfterEachTest`/`ResetMocksAfterEachAsyncTest`. Overrides `createMock` (the non-inline runtime method) to intercept all mock creation.
 */
private[scalatest] trait ResetMocksAfterEachTestCompat extends MockCreatorRuntime with ResetMocksAfterEachTestRuntime {

  abstract override private[mockito] def createMock[T <: AnyRef: ClassTag](
      mockSettings: MockSettings,
      interfaces: List[Class[?]],
      mockHandler: (MockCreationSettings[T], Prettifier) => MockHandler[T]
  )(implicit $pt: Prettifier): T =
    addMock(super.createMock[T](mockSettings, interfaces, mockHandler))
}
