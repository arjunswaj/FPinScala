package com.asb.snl

import scala.annotation.tailrec

/**
  * Stream.
  * Created by arjun on 16/10/16.
  */
sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    @tailrec
    def loop(l: Stream[A], acc: List[A]): List[A] = l match {
      case Empty => acc
      case Cons(h, t) => loop(t(), h() :: acc)
    }
    loop(this, List()).reverse
  }

  def reverse(): Stream[A] = {
    @tailrec
    def loop(tl: Stream[A], acc: => Stream[A]): Stream[A] = tl match {
      case Empty => acc
      case Cons(h, t) => loop(t(), Cons(h, () => acc))
    }
    loop(this, Stream())
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  def takeAsUnfold(n: Int): Stream[A] = Stream.unfold((this, n)) {
    case (Cons(h, t), k) if k > 0 => Some((h(), (t(), k - 1)))
    case _ => None
  }


  // This is incorrect, not lazy
  def take2(n: Int): Stream[A] = {
    @tailrec
    def loop(tl: Stream[A], c: Int, acc: => Stream[A]): Stream[A] = tl match {
      case Empty => acc
      case Cons(h, t) => if (c < n) loop(t(), c + 1, Cons(h, () => acc)) else acc
    }
    loop(this, 0, Stream()).reverse()
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // Okay, but you don't need a loop, it's already tailrec
  def drop2(n: Int): Stream[A] = {
    @tailrec
    def loop(tl: Stream[A], c: Int): Stream[A] = tl match {
      case Empty => tl
      case Cons(h, t) => if (c < n) loop(t(), c + 1) else tl
    }
    loop(this, 0)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  def takeWhileAsUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }


  // Again, incorrect take while, it ain't lazy
  def takeWhile2(p: A => Boolean): Stream[A] = {
    @tailrec
    def loop(tl: Stream[A], acc: => Stream[A]): Stream[A] = tl match {
      case Empty => acc
      case Cons(h, t) => if (p(h())) loop(t(), Cons(h, () => acc)) else acc
    }
    loop(this, Stream()).reverse()
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existByFolding(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhileByFolding(p: A => Boolean): Stream[A] = foldRight(Stream[A]())((a, b) => if (p(a)) Cons(() => a, () => b) else b)

  def headOptionByFolding: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

  def map[B](p: A => B): Stream[B] = foldRight(Stream.empty[B])((a, b) => Stream.cons(p(a), b))

  def mapAsUnfold[B](p: A => B): Stream[B] = Stream.unfold(this) {
    case Cons(h, t) => Some((p(h()), t()))
    case _ => None
  }

  def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)

  def append[B >: A](s2: => Stream[B]): Stream[B] = foldRight(s2)((a, b) => Stream.cons(a, b))

  def flatMap[B](p: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, b) => p(a).append(b))

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), _) => Some((Some(h1()), None), (t1(), Stream.empty))
    case (_, Cons(h2, t2)) => Some((None, Some(h2())), (Stream.empty, t2()))
    case _ => None
  }

  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).takeWhile(k => k._2.isDefined) forAll {
    case (h1, h2) => h1 == h2
  }

  // Last element is empty stream, so appending it manually
  def tails: Stream[Stream[A]] = Stream.unfold(this) {
    case Empty => None
    case s => Some(s, s drop 1)
  } append Stream.empty

  def hasSubsequence[B](s: Stream[B]): Boolean =
    tails exists (t => t startsWith s)
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    lazy val c: Stream[A] = Stream.cons[A](a, c)
    c
  }

  def from(n: Int): Stream[Int] = Stream.cons[Int](n, from(n + 1))

  def fibs(): Stream[Int] = {
    def gen(cur: Int, next: Int): Stream[Int] =
      Stream.cons(cur, gen(next, cur + next))
    gen(0, 1)
  }

  def unfold3[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold3(s)(f))
    case None => Stream.empty
  }

  def unfold2[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map(k => Stream.cons(k._1, unfold2(k._2)(f))).getOrElse(Stream.empty)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).foldRight(Stream.empty[A])((a, b) => Stream.cons(a._1, unfold(a._2)(f)))

  def constantAsUnfold[A](a: A): Stream[A] = unfold(a)(s => Some((a, a)))

  def fromAsUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  def fibsAsUnfold(): Stream[Int] =
    unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2)))

}
