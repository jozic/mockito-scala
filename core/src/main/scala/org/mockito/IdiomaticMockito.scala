package org.mockito

import org.mockito.stubbing.{Answer, DefaultAnswer, ScalaOngoingStubbing}
import org.mockito.MockitoSugar.{verify, _}
import org.mockito.WhenMacro._

import scala.language.experimental.macros
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

trait IdiomaticMockito extends MockCreator {

  override def mock[T <: AnyRef: ClassTag: TypeTag](name: String)(implicit defaultAnswer: DefaultAnswer): T =
    MockitoSugar.mock[T](name)

  override def mock[T <: AnyRef: ClassTag: TypeTag](mockSettings: MockSettings): T = MockitoSugar.mock[T](mockSettings)

  override def mock[T <: AnyRef: ClassTag: TypeTag](defaultAnswer: DefaultAnswer): T =
    MockitoSugar.mock[T](defaultAnswer)

  override def mock[T <: AnyRef: ClassTag: TypeTag](implicit defaultAnswer: DefaultAnswer): T =
    MockitoSugar.mock[T]

  override def spy[T](realObj: T): T = MockitoSugar.spy(realObj)

  override def spyLambda[T <: AnyRef: ClassTag](realObj: T): T = MockitoSugar.spyLambda(realObj)

  implicit class StubbingOps[T](stubbing: => T) {

    def shouldReturn: ReturnActions[T] = macro WhenMacro.shouldReturn[T]

    def shouldCallRealMethod: ScalaOngoingStubbing[T] = macro WhenMacro.shouldCallRealMethod[T]

    def shouldThrow: ThrowActions[T] = macro WhenMacro.shouldThrow[T]

    def shouldAnswer: AnswerActions[T] = macro WhenMacro.shouldAnswer[T]

  }

  class Returned
  class ReturnedBy[R](v: R) {
    def by[M](mock: M): M = doReturn(v).when(mock)
  }

  class Answered
  class AnsweredBy(answer: Answer[Any]) {
    def by[M](mock: M): M = Mockito.doAnswer(answer).when(mock)
  }

  class RealMethod {
    def willBe(called: Called): Called = called
  }
  class Called {
    def by[M](mock: M): M = doCallRealMethod.when(mock)
  }

  class Thrown
  class ThrownBy(v: Throwable) {
    def by[M](mock: M): M = doThrow(v).when(mock)
  }

  val called        = new Called
  val thrown        = new Thrown
  val returned      = new Returned
  val answered      = new Answered
  val theRealMethod = new RealMethod

  implicit class DoSomethingOps[R](v: R) {
    def willBe(r: Returned): ReturnedBy[R] = new ReturnedBy(v)
    def willBe(a: Answered): AnsweredBy = v match {
      case f: Function0[_]                                => new AnsweredBy(invocationToAnswer(_ => f()))
      case f: Function1[_, _]                             => new AnsweredBy(functionToAnswer(f))
      case f: Function2[_, _, _]                          => new AnsweredBy(functionToAnswer(f))
      case f: Function3[_, _, _, _]                       => new AnsweredBy(functionToAnswer(f))
      case f: Function4[_, _, _, _, _]                    => new AnsweredBy(functionToAnswer(f))
      case f: Function5[_, _, _, _, _, _]                 => new AnsweredBy(functionToAnswer(f))
      case f: Function6[_, _, _, _, _, _, _]              => new AnsweredBy(functionToAnswer(f))
      case f: Function7[_, _, _, _, _, _, _, _]           => new AnsweredBy(functionToAnswer(f))
      case f: Function8[_, _, _, _, _, _, _, _, _]        => new AnsweredBy(functionToAnswer(f))
      case f: Function9[_, _, _, _, _, _, _, _, _, _]     => new AnsweredBy(functionToAnswer(f))
      case f: Function10[_, _, _, _, _, _, _, _, _, _, _] => new AnsweredBy(functionToAnswer(f))
      case other                                          => new AnsweredBy(invocationToAnswer(_ => other))
    }
  }

  implicit class ThrowSomethingOps[R <: Throwable](v: R) {
    def willBe(thrown: Thrown): ThrownBy = new ThrownBy(v)
  }

  class On
  class OnlyOn
  class Never
  //noinspection UnitMethodIsParameterless
  case class NeverInstance[T <: AnyRef](mock: T) {
    def called: Unit               = verifyZeroInteractions(mock)
    def called(on: On): T          = verify(mock, MockitoSugar.never)
    def called(again: Again): Unit = verifyNoMoreInteractions(mock)
  }
  class Again
  case class Times(times: Int)
  case class AtLeast(times: Int)
  case class AtMost(times: Int)

  val on                  = new On
  val onlyOn              = new OnlyOn
  val never               = new Never
  val again               = new Again
  val onceOn              = Times(1)
  val twiceOn             = Times(2)
  val thriceOn            = Times(3)
  val threeTimesOn        = Times(3)
  val fourTimesOn         = Times(4)
  val fiveTimesOn         = Times(5)
  val sixTimesOn          = Times(6)
  val sevenTimesOn        = Times(7)
  val eightTimesOn        = Times(8)
  val nineTimesOn         = Times(9)
  val tenTimesOn          = Times(10)
  val atLeastOnceOn       = AtLeast(1)
  val atLeastTwiceOn      = AtLeast(2)
  val atLeastThriceOn     = AtLeast(3)
  val atLeastThreeTimesOn = AtLeast(3)
  val atLeastFourTimesOn  = AtLeast(4)
  val atLeastFiveTimesOn  = AtLeast(5)
  val atLeastSixTimesOn   = AtLeast(6)
  val atLeastSevenTimesOn = AtLeast(7)
  val atLeastEightTimesOn = AtLeast(8)
  val atLeastNineTimesOn  = AtLeast(9)
  val atLeastTenTimesOn   = AtLeast(10)
  val atMostOnceOn        = AtMost(1)
  val atMostTwiceOn       = AtMost(2)
  val atMostThriceOn      = AtMost(3)
  val atMostThreeTimesOn  = AtMost(3)
  val atMostFourTimesOn   = AtMost(4)
  val atMostFiveTimesOn   = AtMost(5)
  val atMostSixTimesOn    = AtMost(6)
  val atMostSevenTimesOn  = AtMost(7)
  val atMostEightTimesOn  = AtMost(8)
  val atMostNineTimesOn   = AtMost(9)
  val atMostTenTimesOn    = AtMost(10)

  implicit class VerificationOps[T <: AnyRef](mock: T)(implicit order: Option[InOrder] = None) {

    def wasCalled(on: On): T = order.fold(verify(mock))(_.verify(mock))

    def wasCalled(t: Times): T = order.fold(verify(mock, times(t.times)))(_.verify(mock, times(t.times)))

    def wasCalled(t: AtLeast): T = order.fold(verify(mock, atLeast(t.times)))(_.verify(mock, times(t.times)))

    def wasCalled(t: AtMost): T = order.fold(verify(mock, atMost(t.times)))(_.verify(mock, times(t.times)))

    def wasCalled(onlyOn: OnlyOn): T = order.fold(verify(mock, only))(_.verify(mock, only))

    def was(n: Never): NeverInstance[T] = NeverInstance(mock)
  }

  object InOrder {
    def apply(mocks: AnyRef*)(verifications: Option[InOrder] => Unit): Unit =
      verifications(Some(Mockito.inOrder(mocks: _*)))
  }
}

/**
 * Simple object to allow the usage of the trait without mixing it in
 */
object IdiomaticMockito extends IdiomaticMockito
