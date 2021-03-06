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

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
    map2(map2(a, b)((aa, bb) => (cc: C) => f(aa, bb, cc)), c)((cToD, c) => cToD(c))

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] =
    map2(map2(map2(a, b)((aa, bb) => (cc: C, dd: D) => f(aa, bb, cc, dd)), c)((cdToE, c) => (dd: D) => cdToE(c, dd)), d)((dToE, d) => dToE(d))

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
    map2(map4(a, b, c, d)((aa, bb, cc, dd) => (ee: E) => f(aa, bb, cc, dd, ee)), e)((eToF, f) => eToF(f))

  def map6[A, B, C, D, E, F, G](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E], f: Par[F])(x: (A, B, C, D, E, F) => G): Par[G] =
    map3(map4(a, b, c, d)((aa, bb, cc, dd) => (ee: E, ff: F) => x(aa, bb, cc, dd, ee, ff)), e, f)((efTog, e, f) => efTog(e, f))

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })

  // Fork that won't fork a new thread, but delays execution until needed.
  def delay[A](a: => Par[A]): Par[A] = es => a(es)

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

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pl: List[Par[List[A]]] = as map async((a: A) => if (f(a)) List(a) else List())
    map(sequenceBalanced(pl))(t => t.flatten)
  }


  def choice[A](p: Par[Boolean])(a: Par[A], b: Par[A]): Par[A] =
    es =>
      if (run(es)(p).get) a(es)
      else b(es)

  def choiceN[A](p: Par[Int])(ls: List[Par[A]]): Par[A] =
    es => ls(run(es)(p).get)(es)

  def choiceAsChoiceN[A](p: Par[Boolean])(a: Par[A], b: Par[A]): Par[A] =
    choiceN(map(p)(if (_) 0 else 1))(List(a, b))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => choices(run(es)(key).get)(es)

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => choices(run(es)(pa).get)(es)

  def choiceAsChooser[A](p: Par[Boolean])(a: Par[A], b: Par[A]): Par[A] =
    chooser(p)(if (_) a else b)

  def choiceNAsChooser[A](p: Par[Int])(ls: List[Par[A]]): Par[A] =
    chooser(p)(ls(_))

  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    join(map(a)(f))

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
