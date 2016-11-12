package com.asb.parallelism

import java.util.concurrent.Executors

import com.asb.parallelism.Par.Par
import com.asb.tests.UnitSpec

/**
  * Summer Test.
  * Created by arjun on 05/11/16.
  */
class SummerTest extends UnitSpec {

  val seq = Seq(1, 2, 3, 4, 5)
  val indexedSeq = IndexedSeq(1, 2, 3, 4, 5)
  val unsortedList = List(5, 3, 2, 1, 4)
  val sortedList = List(1, 2, 3, 4, 5)

  "A Summer" should "sum the values" in {
    summer.sum(seq) shouldEqual 15
  }

  "A Summer" should "sum in divide and conquer as well" in {
    summer.sumDQ(indexedSeq) shouldEqual 15
  }

  it should "be same as original summer" in {
    summer.sum(seq) should equal(summer.sumDQ(indexedSeq))
  }

  "A parallel summer" should "sum the values" in {
    val es = Executors.newFixedThreadPool(3)
    Par.run(es)(summer.sumPar(indexedSeq)).get shouldEqual 15
  }

  "A parallel summer with timeout support" should "sum the values" in {
    val es = Executors.newFixedThreadPool(3)
    Par.run(es)(summer.sumPar2(indexedSeq)).get shouldEqual 15
  }

  "A lazyUnit" should "make any function lazy" in {
    val es = Executors.newFixedThreadPool(3)
    def addFive(a: Int): Int = a + 5
    def asyncAddFive: Int => Par[Int] = Par.async(addFive)

    Par.run(es)(asyncAddFive(5)).get should equal(10)
  }

  "A sorter" should "sort the list" in {
    val es = Executors.newFixedThreadPool(3)

    val unsorted = Par.unit(unsortedList)
    val sorted = Par.sortPar(unsorted)

    Par.run(es)(sorted).get should equal(sortedList)
  }

  "A mapper" should "map the parallel computation" in {
    val parStr: Par[String] = Par.unit("5")
    val parInt: Par[Int] = Par.map(parStr)(_.toInt)
  }

}
