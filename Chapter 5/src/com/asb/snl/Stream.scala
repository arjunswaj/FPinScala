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

  def take(n: Int): Stream[A] = {
    @tailrec
    def loop(tl: Stream[A], c: Int, acc: => Stream[A]): Stream[A] = tl match {
      case Empty => acc
      case Cons(h, t) => if (c < n) loop(t(), c + 1, Cons(h, () => acc)) else acc
    }
    loop(this, 0, Stream()).reverse()
  }

  def drop(n: Int): Stream[A] = {
    @tailrec
    def loop(tl: Stream[A], c: Int): Stream[A] = tl match {
      case Empty => tl
      case Cons(h, t) => if (c < n) loop(t(), c + 1) else tl
    }
    loop(this, 0)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    @tailrec
    def loop(tl: Stream[A], acc: => Stream[A]): Stream[A] = tl match {
      case Empty => acc
      case Cons(h, t) => if (p(h())) loop(t(), Cons(h, () => acc)) else acc
    }
    loop(this, Stream()).reverse()
  }
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

}
