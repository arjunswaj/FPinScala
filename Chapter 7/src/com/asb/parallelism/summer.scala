package com.asb.parallelism

import com.asb.parallelism.Par.Par

/**
  * Created by arjun on 05/11/16.
  */
object summer {

  def sumDQ(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sumDQ(l) + sumDQ(r)
    }


  //noinspection SimplifiableFoldOrReduce
  def sum(ints: Seq[Int]) =
    ints.foldLeft(0)((b, a) => b + a)

  def sumPar(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sumPar(l)), Par.fork(sumPar(r)))(_ + _)
    }

  def sumPar2(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2T(Par.fork(sumPar2(l)), Par.fork(sumPar2(r)))(_ + _)
    }

  def maxPar(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2T(Par.fork(maxPar(l)), Par.fork(maxPar(r)))((a, b) => if (a > b) a else b)
    }

  def wordCount(strs: IndexedSeq[String]): Par[Int] =
    if (strs.size <= 1)
      Par.unit(strs.length)
    else {
      val (l, r) = strs.splitAt(strs.length / 2)
      Par.map2(Par.fork(wordCount(l)), Par.fork(wordCount(r)))(_ + _)
    }

  def nonBlockingWordCount(strs: IndexedSeq[String]): NonBlocking.Par[Int] =
    if (strs.size <= 1)
      NonBlocking.Par.unit(strs.length)
    else {
      val (l, r) = strs.splitAt(strs.length / 2)
      NonBlocking.Par.map2(NonBlocking.Par.fork(nonBlockingWordCount(l)),
        NonBlocking.Par.fork(nonBlockingWordCount(r)))(_ + _)
    }

}
