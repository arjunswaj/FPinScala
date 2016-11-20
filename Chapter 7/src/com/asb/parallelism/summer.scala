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
    ???
}
