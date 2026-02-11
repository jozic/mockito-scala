package org.mockito.internal

import scala.quoted.*

object MacroDebug {
  inline def debugResult[T](inline code: String, inline expr: T): T = expr
}
