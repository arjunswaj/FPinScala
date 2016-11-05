package com.asb.parallelism

/**
  * Created by arjun on 05/11/16.
  */
object summer {

  def sumDQ(ints: Seq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sumDQ(l) + sumDQ(r)
    }


  //noinspection SimplifiableFoldOrReduce
  def sum(ints: Seq[Int]) =
  ints.foldLeft(0)((b, a) => b + a)

}
