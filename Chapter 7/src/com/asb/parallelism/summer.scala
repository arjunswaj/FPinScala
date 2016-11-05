package com.asb.parallelism

/**
  * Created by arjun on 05/11/16.
  */
object summer {

  //noinspection SimplifiableFoldOrReduce
  def sum(ints: Seq[Int]) =
  ints.foldLeft(0)((b, a) => b + a)

}
