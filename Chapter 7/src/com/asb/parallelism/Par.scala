package com.asb.parallelism

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}


/**
  * Parallelism.
  * Created by arjun on 06/11/16.
  */
object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isCancelled: Boolean = false

    override def get(timeout: Long, unit: TimeUnit): A = get

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isDone: Boolean = true
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
}
