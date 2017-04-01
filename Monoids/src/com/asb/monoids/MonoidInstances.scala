package com.asb.monoids

import com.asb.parallelism.NonBlocking.Par

/**
  * Created by arjun on 25/02/17.
  */
object MonoidInstances {

  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero = None
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)

    override def zero: A = m.zero
  }

  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]

  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(f: (A) => A, g: (A) => A): (A) => A = f compose g

    override def zero: (A) => A = (a: A) => a
  }

  val words = List("Hello", "There", "World")
  val s: String = words.foldLeft(stringMonoid.zero)(stringMonoid.op)
  val t: String = words.foldRight(stringMonoid.zero)(stringMonoid.op)

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f(_)).foldLeft(m.zero)(m.op)

  def foldLeft[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(a, b))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = as match {
    case Seq.empty => m.zero
    case a +: Seq.empty => f(a)
    case _ => val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)((a, b) => m.op(a, b))

    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.flatMap(Par.parMap(v)(f)) {
      bs => foldMapV(bs, par(m))(b => Par.lazyUnit(b))
    }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val mon = new Monoid[Option[(Int, Int, Boolean)]] {
      override def op(a1: Option[(Int, Int, Boolean)],
                      a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = (a1, a2) match {
        case (Some((x1, y1, p)), Some((x2, y2, q))) => Some((x1 min x2, y1 min y2, p && q && y1 <= x2))
        case (x, None) => x
        case (None, x) => x
      }

      override def zero: Option[(Int, Int, Boolean)] = None
    }

    foldMapV(ints, mon)(i => Some((i, i, true))).map(o => o._3).getOrElse(true)
  }

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(a1: (A, B), a2: (A, B)): (A, B) = (a.op(a1._1, a2._1), b.op(a1._2, a2._2))

      def zero: (A, B) = (a.zero, b.zero)
    }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(f: A => B, g: A => B): A => B = a => B.op(f(a), g(a))

    def zero: A => B = a => B.zero
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
      (a.keySet ++ b.keySet).foldLeft(zero)((acc, k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero))))

    def zero: Map[K, V] = Map[K, V]()
  }

  val M: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))

}
