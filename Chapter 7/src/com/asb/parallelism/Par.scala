package com.asb.parallelism

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}


/**
  * Parallelism.
  * Created by arjun on 06/11/16.
  */
object Par {

  def map2T[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf, f)
    }


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

  def async[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2T(parList, unit(()))((a, _) => a.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2T(pa, unit(()))((a, _) => f(a))

  def sortParWithGeneralisedMap(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(t => async(f)(t))
    sequence(fbs.reverse)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldLeft(Par.unit(List[A]()))((parOfList, el) => map2T(el, parOfList)(_ :: _))

  def parMapBalanced[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(t => async(f)(t))
    sequenceBalanced(fbs)
  }

  def sequenceBalanced[A](ps: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(ps.toIndexedSeq))(_.toList)

  def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = ps match {
    case IndexedSeq() => unit(Vector())
    case IndexedSeq(x) => map(x)(k => Vector(k))
    case _ => val (l, r) = ps.splitAt(ps.length / 2)
      map2T(sequenceBalanced(l), sequenceBalanced(r))((a, b) => a ++ b)
  }

  case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C)
    extends Future[C] {

    @volatile var cache: Option[C] = None

    override def isCancelled: Boolean = a.isCancelled || b.isCancelled

    override def get(): C = compute(Long.MaxValue)

    override def get(timeout: Long, unit: TimeUnit): C = compute(timeout)

    override def cancel(mayInterruptIfRunning: Boolean): Boolean =
      a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)

    override def isDone: Boolean = cache.isDefined

    private def compute(timeout: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.currentTimeMillis
        val ar = a.get(timeout, TimeUnit.MILLISECONDS)
        val stop = System.currentTimeMillis()
        val at = start - stop
        val br = b.get(timeout - at, TimeUnit.MILLISECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

}
