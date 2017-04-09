package com.asb.monads

import com.asb.pbt.{Gen, Prop}

object MonadInstances {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Prop.unit(a)

    def flatMap[A, B](fa: Gen[A])(f: (A) => Gen[B]): Gen[B] =
      fa flatMap f
  }
}
