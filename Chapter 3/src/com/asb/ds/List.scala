package com.asb.ds

import scala.annotation.tailrec

/**
  * List.
  * Created by arjun on 07/10/16.
  */
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(doubles: List[Double]): Double = doubles match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => throw new Exception("Nil has no tail my dear friend")
    case Cons(y, ys) => ys
  }

  def setHead[A](list: List[A], head: A): List[A] = list match {
    case Nil => Cons(head, Nil)
    case Cons(y, ys) => Cons(head, ys)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (0 >= n) l
    else l match {
      case Nil => throw new Exception("Oh oh! Did you try to remove more than what is present?")
      case Cons(y, ys) => drop(ys, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => l
    case Cons(y, ys) => if (f(y)) dropWhile(ys, f) else l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("Error Duh")
    case Cons(y, Cons(z, Nil)) => Cons(y, Nil)
    case Cons(y, ys) => Cons(y, init(ys))
  }

  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(y, ys) => if (f(y)) dropWhile2(ys)(f) else as
    case _ => as
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(y, ys) => f(y, foldRight(ys, z)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def prod2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((a, b) => 1 + b)


  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(ys: List[A], res: B): B = ys match {
      case Nil => res
      case Cons(b, bs) => go(bs, f(res, b))
    }
    go(as, z)
  }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def prod3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def reverse[A](as: List[A]): List[A] = foldLeft[A, List[A]](as, Nil)((a, b) => Cons(b, a))

  def foldLeftAsFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(as), z)((b, a) => f(a, b))

  def foldRightAsFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))

  def appendRight[A](a1: List[A], a2: List[A]) = foldRight(a1, a2)((a, b) => Cons(a, b))

  def appendLeft[A](a1: List[A], a2: List[A]) = foldLeft(reverse(a1), a2)((b, a) => Cons(a, b))

  def flattenRight[A](ls: List[List[A]]): List[A] = foldRight(ls, Nil: List[A])((a, b) => appendRight(a, b))

  def flattenLeft[A](ls: List[List[A]]): List[A] = foldLeft(ls, Nil: List[A])((b, a) => appendLeft(b, a))

  def addToAllElements(n: Int)(as: List[Int]): List[Int] = foldLeft(reverse(as), Nil: List[Int])((b, a) => Cons(a + n, b))

  val addOneToAllElements = addToAllElements(1) _

  def convertDoubleToString(as: List[Double]): List[String] = foldRight(as, Nil: List[String])((a, b) => Cons(a.toString, b))

  def map[A, B](as: List[A])(f: A => B): List[B] = foldLeft(reverse(as), Nil: List[B])((b, a) => Cons(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldLeft(reverse(as), Nil: List[A])((b, a) => if (f(a)) Cons(a, b) else b)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldLeft(reverse(as), Nil: List[B])((b, a) => appendLeft(f(a), b))

  def filterFromFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  def summer(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Nil, Nil) => Nil
    case (Cons(y, ys), Cons(z, zs)) => Cons(y + z, summer(ys, zs))
    case _ => throw new Exception("Dude, unequal sized lists!")
  }

  def zipWith[A, B, C](a1: List[A], a2: List[B])(f: (A, B) => C): List[C] = {

    @tailrec
    def go(m: List[A], n: List[B], res: List[C]): List[C] = (m, n) match {
      case (Nil, Nil) => res
      case (Cons(y, ys), Cons(z, zs)) => go(ys, zs, Cons(f(y, z), res))
      case _ => throw new Exception("Unequal, you are in length")
    }

    reverse(go(a1, a2, Nil))
  }

  def take[A](as: List[A], n: Int): List[A] = {
    @tailrec
    def go(count: Int, l: List[A], res: List[A]): List[A] =
      if (0 >= count) res
      else l match {
        case Nil => res
        case Cons(y, ys) => go(count - 1, ys, Cons(y, res))
      }
    reverse(go(n, as, Nil))
  }

  def splitAt[A](n: Int)(as: List[A]): (List[A], List[A]) = {
    @tailrec
    def go(count: Int, l: List[A], res: (List[A], List[A])): (List[A], List[A]) =
      if (0 >= count) res
      else l match {
        case Nil => res
        case Cons(y, ys) => go(count - 1, ys, (Cons(y, res._1), ys))
      }
    val res = go(n, as, (Nil, as))
    (reverse(res._1), res._2)
  }

  def takeWhile[A](as: List[A], f: A => Boolean): List[A] = foldLeft(reverse(as), Nil: List[A])((b, a) => if (f(a)) Cons(a, b) else Nil)

  def reverser[A](n: Int)(as: List[A]): List[A] = {
    @tailrec
    def go(bs: List[A], res: List[A]): List[A] = bs match {
      case Nil => res
      case Cons(y, ys) =>
        val split = splitAt(n)(bs)
        go(split._2, appendLeft(res, reverse(split._1)))
    }
    go(as, Nil)
  }

  def forAll[A](as: List[A])(f: A => Boolean): Boolean = foldLeft(as, true)((b, a) => b && f(a))

  def exists[A](as: List[A])(f: A => Boolean): Boolean = foldLeft(as, false)((b, a) => b || f(a))

  def scanLeft[A, B](as: List[A], z: B)(f: (B, A) => B): List[B] = {
    @tailrec
    def go(bs: List[A], res: List[B]): List[B] = (bs, res) match {
      case (Nil, _) => res
      case (Cons(y, ys), Cons(r, rs)) => go(ys, Cons(f(r, y), res))
    }
    go(as, List(z))
  }

  def scanRight[A, B](as: List[A], z: B)(f: (A, B) => B): List[B] = as match {
    case Nil => List(z)
    case Cons(y, ys) =>
      val right = scanRight(ys, z)(f)
      right match {
        case Cons(r, rs) => Cons(f(y, r), right)
      }
  }

  @tailrec
  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(y, ys), Cons(z, zs)) =>
      if (y == z)
        hasSubSequence(ys, zs)
      else
        hasSubSequence(ys, sub)
  }

  @tailrec
  def hasContiguousSequence[A](sup: List[A], sub: List[A]): Boolean = {

    @tailrec
    def startsWith(a1: List[A], a2: List[A]): Boolean = (a1, a2) match {
      case (_, Nil) => true
      case (Cons(m, ms), Cons(n, ns)) => if (m == n) startsWith(ms, ns) else false
      case _ => false
    }

    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(m, ms), _) => if (startsWith(sup, sub)) true else hasContiguousSequence(ms, sub)
    }
  }

}
