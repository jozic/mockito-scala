package org.mockito.scalaz

import scalaz.{ Applicative, MonadError }
import org.mockito.*
import org.mockito.scalaz.IdiomaticMockitoScalazRuntime.*

/**
 * Low-priority willBe answered extensions for generic R values (non-function types). Function-specific extensions in IdiomaticMockitoScalaz take priority over these.
 */
private[mockito] trait LowPriorityScalazDoSomething {
  import org.mockito.scalaz.IdiomaticMockitoScalaz.*

  extension [R](v: R) {
    def willBe(r: AnsweredF.type): AnsweredByF_Value[R]   = AnsweredByF_Value[R](v)
    def willBe(r: AnsweredFG.type): AnsweredByFG_Value[R] = AnsweredByFG_Value[R](v)
  }
}

/**
 * Scala 3 version of IdiomaticMockitoScalaz with inline macro-based stubbing operations.
 */
private[mockito] trait IdiomaticMockitoScalaz extends IdiomaticMockitoScalazRuntime with LowPriorityScalazDoSomething {
  import org.mockito.scalaz.IdiomaticMockitoScalaz.*

  // ---- Stubbing extensions on F[T] ----

  extension [F[_], T](inline stubbing: F[T]) {
    transparent inline def shouldReturnF: ReturnActions[F, T] =
      new ReturnActions[F, T](ScalazStubbing(WhenMacro.whenRaw[F[T]](stubbing)))
    transparent inline def mustReturnF: ReturnActions[F, T] =
      new ReturnActions[F, T](ScalazStubbing(WhenMacro.whenRaw[F[T]](stubbing)))
    transparent inline def returnsF: ReturnActions[F, T] =
      new ReturnActions[F, T](ScalazStubbing(WhenMacro.whenRaw[F[T]](stubbing)))

    transparent inline def shouldFailWith: ThrowActions[F, T] =
      new ThrowActions[F, T](ScalazStubbing(WhenMacro.whenRaw[F[T]](stubbing)))
    transparent inline def mustFailWith: ThrowActions[F, T] =
      new ThrowActions[F, T](ScalazStubbing(WhenMacro.whenRaw[F[T]](stubbing)))
    transparent inline def failsWith: ThrowActions[F, T] =
      new ThrowActions[F, T](ScalazStubbing(WhenMacro.whenRaw[F[T]](stubbing)))
    transparent inline def raises: ThrowActions[F, T] =
      new ThrowActions[F, T](ScalazStubbing(WhenMacro.whenRaw[F[T]](stubbing)))

    transparent inline def shouldAnswerF: AnswerActions[F, T] =
      new AnswerActions[F, T](ScalazStubbing(WhenMacro.whenRaw[F[T]](stubbing)))
    transparent inline def mustAnswerF: AnswerActions[F, T] =
      new AnswerActions[F, T](ScalazStubbing(WhenMacro.whenRaw[F[T]](stubbing)))
    transparent inline def answersF: AnswerActions[F, T] =
      new AnswerActions[F, T](ScalazStubbing(WhenMacro.whenRaw[F[T]](stubbing)))
  }

  // ---- Stubbing extensions on F[G[T]] ----

  extension [F[_], G[_], T](inline stubbing: F[G[T]]) {
    transparent inline def shouldReturnFG: ReturnActions2[F, G, T] =
      new ReturnActions2[F, G, T](ScalazStubbing2(WhenMacro.whenRaw[F[G[T]]](stubbing)))
    transparent inline def mustReturnFG: ReturnActions2[F, G, T] =
      new ReturnActions2[F, G, T](ScalazStubbing2(WhenMacro.whenRaw[F[G[T]]](stubbing)))
    transparent inline def returnsFG: ReturnActions2[F, G, T] =
      new ReturnActions2[F, G, T](ScalazStubbing2(WhenMacro.whenRaw[F[G[T]]](stubbing)))

    transparent inline def shouldFailWithG: ThrowActions2[F, G, T] =
      new ThrowActions2[F, G, T](ScalazStubbing2(WhenMacro.whenRaw[F[G[T]]](stubbing)))
    transparent inline def mustFailWithG: ThrowActions2[F, G, T] =
      new ThrowActions2[F, G, T](ScalazStubbing2(WhenMacro.whenRaw[F[G[T]]](stubbing)))
    transparent inline def failsWithG: ThrowActions2[F, G, T] =
      new ThrowActions2[F, G, T](ScalazStubbing2(WhenMacro.whenRaw[F[G[T]]](stubbing)))
    transparent inline def raisesG: ThrowActions2[F, G, T] =
      new ThrowActions2[F, G, T](ScalazStubbing2(WhenMacro.whenRaw[F[G[T]]](stubbing)))

    transparent inline def shouldAnswerFG: AnswerActions2[F, G, T] =
      new AnswerActions2[F, G, T](ScalazStubbing2(WhenMacro.whenRaw[F[G[T]]](stubbing)))
    transparent inline def mustAnswerFG: AnswerActions2[F, G, T] =
      new AnswerActions2[F, G, T](ScalazStubbing2(WhenMacro.whenRaw[F[G[T]]](stubbing)))
    transparent inline def answersFG: AnswerActions2[F, G, T] =
      new AnswerActions2[F, G, T](ScalazStubbing2(WhenMacro.whenRaw[F[G[T]]](stubbing)))
  }

  // ---- DoSomething extensions: willBe returnedF/returnedFG/raised/raisedG ----

  extension [R](v: R) {
    def willBe(r: ReturnedF.type): ReturnedByF[R]   = ReturnedByF[R](v)
    def willBe(r: ReturnedFG.type): ReturnedByFG[R] = ReturnedByFG[R](v)
    def willBe(r: Raised.type): RaisedBy[R]         = RaisedBy[R](v)
    def willBe(r: RaisedG.type): RaisedByG[R]       = RaisedByG[R](v)
  }

  // ---- DoSomething extensions: willBe answeredF/answeredFG ----
  // Function-specific extensions (higher priority than generic R in LowPriorityScalazDoSomething)

  extension [R](v: () => R) {
    def willBe(a: AnsweredF.type): AnsweredByF_Fun0[R]   = AnsweredByF_Fun0[R](v)
    def willBe(a: AnsweredFG.type): AnsweredByFG_Fun0[R] = AnsweredByFG_Fun0[R](v)
  }

  extension [P0, R](v: P0 => R) {
    def willBe(a: AnsweredF.type): AnsweredByF_Fun1[P0, R]   = AnsweredByF_Fun1[P0, R](v)
    def willBe(a: AnsweredFG.type): AnsweredByFG_Fun1[P0, R] = AnsweredByFG_Fun1[P0, R](v)
  }

  extension [P0, P1, R](v: (P0, P1) => R) {
    def willBe(a: AnsweredF.type): AnsweredByF_Fun2[P0, P1, R]   = AnsweredByF_Fun2[P0, P1, R](v)
    def willBe(a: AnsweredFG.type): AnsweredByFG_Fun2[P0, P1, R] = AnsweredByFG_Fun2[P0, P1, R](v)
  }

  extension [P0, P1, P2, R](v: (P0, P1, P2) => R) {
    def willBe(a: AnsweredF.type): AnsweredByF_Fun3[P0, P1, P2, R]   = AnsweredByF_Fun3[P0, P1, P2, R](v)
    def willBe(a: AnsweredFG.type): AnsweredByFG_Fun3[P0, P1, P2, R] = AnsweredByFG_Fun3[P0, P1, P2, R](v)
  }

  extension [P0, P1, P2, P3, R](v: (P0, P1, P2, P3) => R) {
    def willBe(a: AnsweredF.type): AnsweredByF_Fun4[P0, P1, P2, P3, R]   = AnsweredByF_Fun4[P0, P1, P2, P3, R](v)
    def willBe(a: AnsweredFG.type): AnsweredByFG_Fun4[P0, P1, P2, P3, R] = AnsweredByFG_Fun4[P0, P1, P2, P3, R](v)
  }

  extension [P0, P1, P2, P3, P4, R](v: (P0, P1, P2, P3, P4) => R) {
    def willBe(a: AnsweredF.type): AnsweredByF_Fun5[P0, P1, P2, P3, P4, R]   = AnsweredByF_Fun5[P0, P1, P2, P3, P4, R](v)
    def willBe(a: AnsweredFG.type): AnsweredByFG_Fun5[P0, P1, P2, P3, P4, R] = AnsweredByFG_Fun5[P0, P1, P2, P3, P4, R](v)
  }

  extension [P0, P1, P2, P3, P4, P5, R](v: (P0, P1, P2, P3, P4, P5) => R) {
    def willBe(a: AnsweredF.type): AnsweredByF_Fun6[P0, P1, P2, P3, P4, P5, R]   = AnsweredByF_Fun6[P0, P1, P2, P3, P4, P5, R](v)
    def willBe(a: AnsweredFG.type): AnsweredByFG_Fun6[P0, P1, P2, P3, P4, P5, R] = AnsweredByFG_Fun6[P0, P1, P2, P3, P4, P5, R](v)
  }

  extension [P0, P1, P2, P3, P4, P5, P6, R](v: (P0, P1, P2, P3, P4, P5, P6) => R) {
    def willBe(a: AnsweredF.type): AnsweredByF_Fun7[P0, P1, P2, P3, P4, P5, P6, R]   = AnsweredByF_Fun7[P0, P1, P2, P3, P4, P5, P6, R](v)
    def willBe(a: AnsweredFG.type): AnsweredByFG_Fun7[P0, P1, P2, P3, P4, P5, P6, R] = AnsweredByFG_Fun7[P0, P1, P2, P3, P4, P5, P6, R](v)
  }

  extension [P0, P1, P2, P3, P4, P5, P6, P7, R](v: (P0, P1, P2, P3, P4, P5, P6, P7) => R) {
    def willBe(a: AnsweredF.type): AnsweredByF_Fun8[P0, P1, P2, P3, P4, P5, P6, P7, R]   = AnsweredByF_Fun8[P0, P1, P2, P3, P4, P5, P6, P7, R](v)
    def willBe(a: AnsweredFG.type): AnsweredByFG_Fun8[P0, P1, P2, P3, P4, P5, P6, P7, R] = AnsweredByFG_Fun8[P0, P1, P2, P3, P4, P5, P6, P7, R](v)
  }

  extension [P0, P1, P2, P3, P4, P5, P6, P7, P8, R](v: (P0, P1, P2, P3, P4, P5, P6, P7, P8) => R) {
    def willBe(a: AnsweredF.type): AnsweredByF_Fun9[P0, P1, P2, P3, P4, P5, P6, P7, P8, R]   = AnsweredByF_Fun9[P0, P1, P2, P3, P4, P5, P6, P7, P8, R](v)
    def willBe(a: AnsweredFG.type): AnsweredByFG_Fun9[P0, P1, P2, P3, P4, P5, P6, P7, P8, R] = AnsweredByFG_Fun9[P0, P1, P2, P3, P4, P5, P6, P7, P8, R](v)
  }

  extension [P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, R](v: (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9) => R) {
    def willBe(a: AnsweredF.type): AnsweredByF_Fun10[P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, R]   = AnsweredByF_Fun10[P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, R](v)
    def willBe(a: AnsweredFG.type): AnsweredByFG_Fun10[P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, R] = AnsweredByFG_Fun10[P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, R](v)
  }
}

object IdiomaticMockitoScalaz extends IdiomaticMockitoScalaz {
  import scala.reflect.ClassTag

  // ---- ReturnedByF / ReturnedByFG ----

  case class ReturnedByF[T](value: T) {
    transparent inline def by[F[_], S](inline stubbing: F[S])(using F: Applicative[F], ev: T <:< S): F[S] =
      DoSomethingMacro.doSomethingBy[F[S]](
        Mockito.doReturn(F.pure(ev(value))),
        stubbing
      )
  }

  case class ReturnedByFG[T](value: T) {
    transparent inline def by[F[_], G[_], S](inline stubbing: F[G[S]])(using F: Applicative[F], G: Applicative[G], ev: T <:< S): F[G[S]] =
      DoSomethingMacro.doSomethingBy[F[G[S]]](
        Mockito.doReturn(F.compose[G].pure(ev(value))),
        stubbing
      )
  }

  // ---- RaisedBy / RaisedByG ----

  case class RaisedBy[T](value: T) {
    transparent inline def by[F[_], E](inline stubbing: F[E])(using F: MonadError[F, ? >: T]): F[E] =
      DoSomethingMacro.doSomethingBy[F[E]](
        Mockito.doReturn(F.raiseError[E](value)),
        stubbing
      )
  }

  case class RaisedByG[T](value: T) {
    transparent inline def by[F[_], G[_], E](inline stubbing: F[G[E]])(using F: Applicative[F], G: MonadError[G, ? >: T]): F[G[E]] =
      DoSomethingMacro.doSomethingBy[F[G[E]]](
        Mockito.doReturn(F.pure(G.raiseError[E](value))),
        stubbing
      )
  }

  // ---- AnsweredByF per-arity case classes ----

  case class AnsweredByF_Value[R](value: R) {
    transparent inline def by[F[_], S](inline stubbing: F[S])(using F: Applicative[F], ev: R <:< S): F[S] =
      DoSomethingMacro.doSomethingBy[F[S]](
        MockitoScalaz.doAnswerF[F, S](ev(value)),
        stubbing
      )
  }

  case class AnsweredByF_Fun0[R](value: () => R) {
    transparent inline def by[F[_], S](inline stubbing: F[S])(using F: Applicative[F], ev: R <:< S): F[S] =
      DoSomethingMacro.doSomethingBy[F[S]](
        MockitoScalaz.doAnswerF[F, S](ev(value())),
        stubbing
      )
  }

  case class AnsweredByF_Fun1[P0, R](value: P0 => R) {
    transparent inline def by[F[_], S](inline stubbing: F[S])(using F: Applicative[F], ev: R <:< S, classTag: ClassTag[P0] = defaultClassTag[P0]): F[S] =
      DoSomethingMacro.doSomethingBy[F[S]](
        MockitoScalaz.doAnswerF[F, P0, S](value.andThen(ev)),
        stubbing
      )
  }

  case class AnsweredByF_Fun2[P0, P1, R](value: (P0, P1) => R) {
    transparent inline def by[F[_], S](inline stubbing: F[S])(using F: Applicative[F], ev: R <:< S): F[S] =
      DoSomethingMacro.doSomethingBy[F[S]](
        MockitoScalaz.doAnswerF[F, P0, P1, S]((p0: P0, p1: P1) => ev(value(p0, p1))),
        stubbing
      )
  }

  case class AnsweredByF_Fun3[P0, P1, P2, R](value: (P0, P1, P2) => R) {
    transparent inline def by[F[_], S](inline stubbing: F[S])(using F: Applicative[F], ev: R <:< S): F[S] =
      DoSomethingMacro.doSomethingBy[F[S]](
        MockitoScalaz.doAnswerF[F, P0, P1, P2, S]((p0: P0, p1: P1, p2: P2) => ev(value(p0, p1, p2))),
        stubbing
      )
  }

  case class AnsweredByF_Fun4[P0, P1, P2, P3, R](value: (P0, P1, P2, P3) => R) {
    transparent inline def by[F[_], S](inline stubbing: F[S])(using F: Applicative[F], ev: R <:< S): F[S] =
      DoSomethingMacro.doSomethingBy[F[S]](
        MockitoScalaz.doAnswerF[F, P0, P1, P2, P3, S]((p0: P0, p1: P1, p2: P2, p3: P3) => ev(value(p0, p1, p2, p3))),
        stubbing
      )
  }

  case class AnsweredByF_Fun5[P0, P1, P2, P3, P4, R](value: (P0, P1, P2, P3, P4) => R) {
    transparent inline def by[F[_], S](inline stubbing: F[S])(using F: Applicative[F], ev: R <:< S): F[S] =
      DoSomethingMacro.doSomethingBy[F[S]](
        MockitoScalaz.doAnswerF[F, P0, P1, P2, P3, P4, S]((p0: P0, p1: P1, p2: P2, p3: P3, p4: P4) => ev(value(p0, p1, p2, p3, p4))),
        stubbing
      )
  }

  case class AnsweredByF_Fun6[P0, P1, P2, P3, P4, P5, R](value: (P0, P1, P2, P3, P4, P5) => R) {
    transparent inline def by[F[_], S](inline stubbing: F[S])(using F: Applicative[F], ev: R <:< S): F[S] =
      DoSomethingMacro.doSomethingBy[F[S]](
        MockitoScalaz.doAnswerF[F, P0, P1, P2, P3, P4, P5, S]((p0: P0, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5) => ev(value(p0, p1, p2, p3, p4, p5))),
        stubbing
      )
  }

  case class AnsweredByF_Fun7[P0, P1, P2, P3, P4, P5, P6, R](value: (P0, P1, P2, P3, P4, P5, P6) => R) {
    transparent inline def by[F[_], S](inline stubbing: F[S])(using F: Applicative[F], ev: R <:< S): F[S] =
      DoSomethingMacro.doSomethingBy[F[S]](
        MockitoScalaz.doAnswerF[F, P0, P1, P2, P3, P4, P5, P6, S]((p0: P0, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6) => ev(value(p0, p1, p2, p3, p4, p5, p6))),
        stubbing
      )
  }

  case class AnsweredByF_Fun8[P0, P1, P2, P3, P4, P5, P6, P7, R](value: (P0, P1, P2, P3, P4, P5, P6, P7) => R) {
    transparent inline def by[F[_], S](inline stubbing: F[S])(using F: Applicative[F], ev: R <:< S): F[S] =
      DoSomethingMacro.doSomethingBy[F[S]](
        MockitoScalaz.doAnswerF[F, P0, P1, P2, P3, P4, P5, P6, P7, S]((p0: P0, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7) =>
          ev(value(p0, p1, p2, p3, p4, p5, p6, p7))
        ),
        stubbing
      )
  }

  case class AnsweredByF_Fun9[P0, P1, P2, P3, P4, P5, P6, P7, P8, R](value: (P0, P1, P2, P3, P4, P5, P6, P7, P8) => R) {
    transparent inline def by[F[_], S](inline stubbing: F[S])(using F: Applicative[F], ev: R <:< S): F[S] =
      DoSomethingMacro.doSomethingBy[F[S]](
        MockitoScalaz.doAnswerF[F, P0, P1, P2, P3, P4, P5, P6, P7, P8, S]((p0: P0, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8) =>
          ev(value(p0, p1, p2, p3, p4, p5, p6, p7, p8))
        ),
        stubbing
      )
  }

  case class AnsweredByF_Fun10[P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, R](value: (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9) => R) {
    transparent inline def by[F[_], S](inline stubbing: F[S])(using F: Applicative[F], ev: R <:< S): F[S] =
      DoSomethingMacro.doSomethingBy[F[S]](
        MockitoScalaz.doAnswerF[F, P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, S]((p0: P0, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9) =>
          ev(value(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9))
        ),
        stubbing
      )
  }

  // ---- AnsweredByFG per-arity case classes ----

  case class AnsweredByFG_Value[R](value: R) {
    transparent inline def by[F[_], G[_], S](inline stubbing: F[G[S]])(using F: Applicative[F], G: Applicative[G], ev: R <:< S): F[G[S]] =
      DoSomethingMacro.doSomethingBy[F[G[S]]](
        MockitoScalaz.doAnswerFG[F, G, S](ev(value)),
        stubbing
      )
  }

  case class AnsweredByFG_Fun0[R](value: () => R) {
    transparent inline def by[F[_], G[_], S](inline stubbing: F[G[S]])(using F: Applicative[F], G: Applicative[G], ev: R <:< S): F[G[S]] =
      DoSomethingMacro.doSomethingBy[F[G[S]]](
        MockitoScalaz.doAnswerFG[F, G, S](ev(value())),
        stubbing
      )
  }

  case class AnsweredByFG_Fun1[P0, R](value: P0 => R) {
    transparent inline def by[F[_], G[_], S](
        inline stubbing: F[G[S]]
    )(using F: Applicative[F], G: Applicative[G], ev: R <:< S, classTag: ClassTag[P0] = defaultClassTag[P0]): F[G[S]] =
      DoSomethingMacro.doSomethingBy[F[G[S]]](
        MockitoScalaz.doAnswerFG[F, G, P0, S](value.andThen(ev)),
        stubbing
      )
  }

  case class AnsweredByFG_Fun2[P0, P1, R](value: (P0, P1) => R) {
    transparent inline def by[F[_], G[_], S](inline stubbing: F[G[S]])(using F: Applicative[F], G: Applicative[G], ev: R <:< S): F[G[S]] =
      DoSomethingMacro.doSomethingBy[F[G[S]]](
        MockitoScalaz.doAnswerFG[F, G, P0, P1, S]((p0: P0, p1: P1) => ev(value(p0, p1))),
        stubbing
      )
  }

  case class AnsweredByFG_Fun3[P0, P1, P2, R](value: (P0, P1, P2) => R) {
    transparent inline def by[F[_], G[_], S](inline stubbing: F[G[S]])(using F: Applicative[F], G: Applicative[G], ev: R <:< S): F[G[S]] =
      DoSomethingMacro.doSomethingBy[F[G[S]]](
        MockitoScalaz.doAnswerFG[F, G, P0, P1, P2, S]((p0: P0, p1: P1, p2: P2) => ev(value(p0, p1, p2))),
        stubbing
      )
  }

  case class AnsweredByFG_Fun4[P0, P1, P2, P3, R](value: (P0, P1, P2, P3) => R) {
    transparent inline def by[F[_], G[_], S](inline stubbing: F[G[S]])(using F: Applicative[F], G: Applicative[G], ev: R <:< S): F[G[S]] =
      DoSomethingMacro.doSomethingBy[F[G[S]]](
        MockitoScalaz.doAnswerFG[F, G, P0, P1, P2, P3, S]((p0: P0, p1: P1, p2: P2, p3: P3) => ev(value(p0, p1, p2, p3))),
        stubbing
      )
  }

  case class AnsweredByFG_Fun5[P0, P1, P2, P3, P4, R](value: (P0, P1, P2, P3, P4) => R) {
    transparent inline def by[F[_], G[_], S](inline stubbing: F[G[S]])(using F: Applicative[F], G: Applicative[G], ev: R <:< S): F[G[S]] =
      DoSomethingMacro.doSomethingBy[F[G[S]]](
        MockitoScalaz.doAnswerFG[F, G, P0, P1, P2, P3, P4, S]((p0: P0, p1: P1, p2: P2, p3: P3, p4: P4) => ev(value(p0, p1, p2, p3, p4))),
        stubbing
      )
  }

  case class AnsweredByFG_Fun6[P0, P1, P2, P3, P4, P5, R](value: (P0, P1, P2, P3, P4, P5) => R) {
    transparent inline def by[F[_], G[_], S](inline stubbing: F[G[S]])(using F: Applicative[F], G: Applicative[G], ev: R <:< S): F[G[S]] =
      DoSomethingMacro.doSomethingBy[F[G[S]]](
        MockitoScalaz.doAnswerFG[F, G, P0, P1, P2, P3, P4, P5, S]((p0: P0, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5) => ev(value(p0, p1, p2, p3, p4, p5))),
        stubbing
      )
  }

  case class AnsweredByFG_Fun7[P0, P1, P2, P3, P4, P5, P6, R](value: (P0, P1, P2, P3, P4, P5, P6) => R) {
    transparent inline def by[F[_], G[_], S](inline stubbing: F[G[S]])(using F: Applicative[F], G: Applicative[G], ev: R <:< S): F[G[S]] =
      DoSomethingMacro.doSomethingBy[F[G[S]]](
        MockitoScalaz.doAnswerFG[F, G, P0, P1, P2, P3, P4, P5, P6, S]((p0: P0, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6) => ev(value(p0, p1, p2, p3, p4, p5, p6))),
        stubbing
      )
  }

  case class AnsweredByFG_Fun8[P0, P1, P2, P3, P4, P5, P6, P7, R](value: (P0, P1, P2, P3, P4, P5, P6, P7) => R) {
    transparent inline def by[F[_], G[_], S](inline stubbing: F[G[S]])(using F: Applicative[F], G: Applicative[G], ev: R <:< S): F[G[S]] =
      DoSomethingMacro.doSomethingBy[F[G[S]]](
        MockitoScalaz.doAnswerFG[F, G, P0, P1, P2, P3, P4, P5, P6, P7, S]((p0: P0, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7) =>
          ev(value(p0, p1, p2, p3, p4, p5, p6, p7))
        ),
        stubbing
      )
  }

  case class AnsweredByFG_Fun9[P0, P1, P2, P3, P4, P5, P6, P7, P8, R](value: (P0, P1, P2, P3, P4, P5, P6, P7, P8) => R) {
    transparent inline def by[F[_], G[_], S](inline stubbing: F[G[S]])(using F: Applicative[F], G: Applicative[G], ev: R <:< S): F[G[S]] =
      DoSomethingMacro.doSomethingBy[F[G[S]]](
        MockitoScalaz.doAnswerFG[F, G, P0, P1, P2, P3, P4, P5, P6, P7, P8, S]((p0: P0, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8) =>
          ev(value(p0, p1, p2, p3, p4, p5, p6, p7, p8))
        ),
        stubbing
      )
  }

  case class AnsweredByFG_Fun10[P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, R](value: (P0, P1, P2, P3, P4, P5, P6, P7, P8, P9) => R) {
    transparent inline def by[F[_], G[_], S](inline stubbing: F[G[S]])(using F: Applicative[F], G: Applicative[G], ev: R <:< S): F[G[S]] =
      DoSomethingMacro.doSomethingBy[F[G[S]]](
        MockitoScalaz.doAnswerFG[F, G, P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, S]((p0: P0, p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9) =>
          ev(value(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9))
        ),
        stubbing
      )
  }
}
