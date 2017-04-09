package com.asb.monads

import com.asb.ds.List
import com.asb.error.{Option, Some}
import com.asb.parallelism.NonBlocking.Par
import com.asb.pbt.{Gen, Prop}
import com.asb.snl.Stream

object MonadInstances {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Prop.unit(a)

    def flatMap[A, B](fa: Gen[A])(f: (A) => Gen[B]): Gen[B] =
      fa flatMap f
  }

  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)

    def flatMap[A, B](fa: Par[A])(f: (A) => Par[B]): Par[B] =
      Par.flatMap(fa)(f)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)

    def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] =
      fa flatMap f
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)

    def flatMap[A, B](fa: Stream[A])(f: (A) => Stream[B]): Stream[B] =
      fa flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)

    def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] =
      List.flatMap(fa)(f)
  }

}
