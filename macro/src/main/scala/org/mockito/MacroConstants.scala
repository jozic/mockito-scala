package org.mockito

import scala.util.matching.Regex

/**
 * Constants used by macro implementations. Shared across Scala 2 and Scala 3.
 */
object MacroConstants {

  /** Set of Mockito matcher method names recognized during macro expansion */
  val MockitoMatchers: Set[String] = Set(
    "anyByte",
    "anyBoolean",
    "anyChar",
    "anyDouble",
    "anyInt",
    "anyFloat",
    "anyShort",
    "anyLong",
    "anyList",
    "anySeq",
    "anyIterable",
    "anySet",
    "anyMap",
    "any",
    "anyVal",
    "$times", // *
    "isNull",
    "isNotNull",
    "eqTo",
    "eqToVal",
    "same",
    "isA",
    "refEq",
    "function0",
    "matches",
    "startsWith",
    "contains",
    "endsWith",
    "argThat",
    "byteThat",
    "booleanThat",
    "charThat",
    "doubleThat",
    "intThat",
    "floatThat",
    "shortThat",
    "longThat",
    "argMatching",
    "$greater",    // >
    "$greater$eq", // >=
    "$less",       // <
    "$less$eq",    // <=
    "$eq$tilde",   // =~
    "Captor.asCapture",
    "capture"
  )

  /** Regex pattern for Specs2 implicit conversion methods */
  val Specs2ImplicitsPattern: Regex = "(matcher)?[t,T]o(Partial)?FunctionCall(\\d*)".r

  /** Check if a method name matches the Specs2 matcher pattern */
  def isSpecs2Matcher(methodName: String): Boolean =
    Specs2ImplicitsPattern.pattern.matcher(methodName).matches

  /** Mapping from words to numbers for Specs2 verification DSL */
  val WordsToNumbers: Map[String, Int] = Map(
    "no"    -> 0,
    "one"   -> 1,
    "two"   -> 2,
    "three" -> 3
  )

  /** Regex pattern for "was" or "were" in Specs2 DSL */
  val WasWerePattern: Regex = "(was|were)".r
}
