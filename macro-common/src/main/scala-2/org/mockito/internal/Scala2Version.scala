package org.mockito.internal

import scala.util.Properties

sealed trait Scala2Version
object Scala2Version {
  case object V2_12 extends Scala2Version
  case object V2_13 extends Scala2Version

  val Current: Scala2Version = {
    val version = Properties.scalaPropOrElse("version.number", "unknown")
    if (version.startsWith("2.12")) Scala2Version.V2_12
    else if (version.startsWith("2.13")) Scala2Version.V2_13
    else throw new Exception(s"Unsupported scala version $version")
  }
}
