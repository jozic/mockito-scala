package org.mockito

import org.mockito.captor.Captor
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Fixture for Scala 3-specific macro tests. */
private trait MacroSub_Service {
  def value(): Int
  def ping(): Unit
  def transform(s: String): Int
}

private case class MacroSub_UserId(value: Long) extends AnyVal

private trait MacroSub_UserService {
  def userId(): MacroSub_UserId
}

class MacroSubScala3Test extends AnyWordSpec with Matchers {

  "WhenMacro.whenRaw" should {
    "stub a plain method invocation" in {
      val service = Mockito.mock(classOf[MacroSub_Service])
      WhenMacro.whenRaw(service.value()).thenReturn(7)

      service.value() shouldBe 7
    }

    "preserve ArgumentMatcher and stub all matching calls" in {
      val service = Mockito.mock(classOf[MacroSub_Service])
      // any() is recognised as a matcher by the macro (no DefaultMatcher wrapping needed)
      WhenMacro.whenRaw(service.transform(ArgumentMatchers.any())).thenReturn(42)

      service.transform("hello") shouldBe 42
      service.transform("world") shouldBe 42
    }
  }

  "DoSomethingMacro" should {
    "stub return values via returnedByMacro" in {
      val service = Mockito.mock(classOf[MacroSub_Service])
      DoSomethingMacro.returnedByMacro(11, service.value())

      service.value() shouldBe 11
    }

    "stub Unit methods via doesNothing" in {
      val service = Mockito.mock(classOf[MacroSub_Service])
      DoSomethingMacro.doesNothing(service.ping())

      service.ping()
      Mockito.verify(service).ping()
    }

    "stub exception throwing via thrownByMacro" in {
      val service = Mockito.mock(classOf[MacroSub_Service])
      DoSomethingMacro.thrownByMacro(new RuntimeException("boom"), service.value())

      val ex = intercept[RuntimeException](service.value())
      ex.getMessage shouldBe "boom"
    }

    "stub with a function via answeredByMacro" in {
      val service = Mockito.mock(classOf[MacroSub_Service])
      // any() is a matcher → no DefaultMatcher wrapping in the template invocation
      DoSomethingMacro.answeredByMacro((_: String).length, service.transform(ArgumentMatchers.any()))

      service.transform("hello") shouldBe 5
      service.transform("hi") shouldBe 2
    }

    "stub a method returning a value class via returnedByMacro" in {
      val service = Mockito.mock(classOf[MacroSub_UserService])
      DoSomethingMacro.returnedByMacro(MacroSub_UserId(42L), service.userId())

      service.userId() shouldBe MacroSub_UserId(42L)
    }
  }

  "Captor" should {
    "use ValueClassCaptor for value classes" in {
      summon[Captor[MacroSub_UserId]].getClass.getSimpleName should include("ValueClassCaptor")
    }
  }
}
