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

  // 7.09: With current implementation of the fork, all these below fixed thread pool
  // are bound to be deadlocked. Read book

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


  "A better sorter" should "sort the list as well" in {

    val es = Executors.newFixedThreadPool(2)

    val unsorted = Par.unit(unsortedList)
    val sorted = Par.sortParWithGeneralisedMap(unsorted)

    Par.run(es)(sorted).get shouldEqual sortedList
  }

  "A parMap" should "map in parallel" in {
    val es = Executors.newFixedThreadPool(2)

    Par.run(es)(Par.parMap(sortedList)(_ + 5)).get shouldEqual List(6, 7, 8, 9, 10)
  }

  "A balanced parMap" should "map in parallel as well" in {
    val es = Executors.newFixedThreadPool(2)
    Par.run(es)(Par.parMapBalanced(sortedList)(_ + 2)).get shouldEqual List(3, 4, 5, 6, 7)
  }

  "A parFilter" should "filter the elements of a list in parallel" in {
    val es = Executors.newFixedThreadPool(5)
    Par.run(es)(Par.parFilter(sortedList)(0 == _ % 2)).get shouldEqual List(2, 4)
    Par.run(es)(Par.parFilter(sortedList)(0 != _ % 2)).get shouldEqual List(1, 3, 5)
  }

  "A max val" should "give the maximum value in the list in parallel" in {
    val es = Executors.newFixedThreadPool(5)
    Par.run(es)(summer.maxPar(IndexedSeq())).get shouldEqual 0
    Par.run(es)(summer.maxPar(IndexedSeq(10))).get shouldEqual 10
    Par.run(es)(summer.maxPar(indexedSeq)).get shouldEqual 5
  }

  "A word counter" should "give the count of all the words" in {
    val es = Executors.newCachedThreadPool()
    Par.run(es)(summer.wordCount(IndexedSeq())).get shouldEqual 0
    Par.run(es)(summer.wordCount("Hola".split(" ").toList.toIndexedSeq)).get shouldEqual 1
    Par.run(es)(summer.wordCount("Times they are a changin".split(" ").toList.toIndexedSeq)).get shouldEqual 5
  }

  "A choice" should "give the right choice. Duh" in {
    val es = Executors.newCachedThreadPool()
    val cond = Par.unit(false)
    val a = summer.wordCount("Times they are a changin Dylan".split(" ").toList.toIndexedSeq)
    val b = summer.wordCount("Times they are a changin".split(" ").toList.toIndexedSeq)
    Par.run(es)(Par.choice(cond)(a, b)).get shouldEqual 5
  }

  "A n choice" should "give the nth choice" in {
    val es = Executors.newCachedThreadPool()
    val choice = Par.unit(1)
    val a = summer.wordCount("Times they are a changin Bob Dylan".split(" ").toList.toIndexedSeq)
    val b = summer.wordCount("Times they are a changin Dylan".split(" ").toList.toIndexedSeq)
    val c = summer.wordCount("Times they are a changin".split(" ").toList.toIndexedSeq)

    Par.run(es)(Par.choiceN(choice)(List(a, b, c))).get shouldEqual 6
  }

  "A choiceAsChoiceN" should "give the right choice. Duh" in {
    val es = Executors.newCachedThreadPool()
    val cond = Par.unit(false)
    val a = summer.wordCount("Times they are a changin Dylan".split(" ").toList.toIndexedSeq)
    val b = summer.wordCount("Times they are a changin".split(" ").toList.toIndexedSeq)
    Par.run(es)(Par.choiceAsChoiceN(cond)(a, b)).get shouldEqual 5
  }

  "A choiceMap" should "give the right choice" in {
    val es = Executors.newCachedThreadPool()
    val key = Par.unit("Dylan")
    val a = summer.wordCount("Times they are a changin Dylan".split(" ").toList.toIndexedSeq)
    val b = summer.wordCount("Times they are a changin".split(" ").toList.toIndexedSeq)
    Par.run(es)(Par.choiceMap[String, Int](key)(Map(("Dylan", a), ("Anon", b)))).get shouldEqual 6
  }

  "A chooser" should "give the right choice" in {
    val es = Executors.newCachedThreadPool()
    val key = Par.unit("Dylan")
    val a = summer.wordCount("Times they are a changin Dylan".split(" ").toList.toIndexedSeq)
    val b = summer.wordCount("Times they are a changin".split(" ").toList.toIndexedSeq)

    Par.run(es)(Par.chooser[String, Int](key)(t => if (t.equals("Dylan")) a else b)).get shouldEqual 6
  }

  "A choiceAsChooser" should "give the right choice. Duh" in {
    val es = Executors.newCachedThreadPool()
    val cond = Par.unit(false)
    val a = summer.wordCount("Times they are a changin Dylan".split(" ").toList.toIndexedSeq)
    val b = summer.wordCount("Times they are a changin".split(" ").toList.toIndexedSeq)
    Par.run(es)(Par.choiceAsChooser(cond)(a, b)).get shouldEqual 5
  }

  "A n choice as Chooser" should "give the nth choice" in {
    val es = Executors.newCachedThreadPool()
    val choice = Par.unit(1)
    val a = summer.wordCount("Times they are a changin Bob Dylan".split(" ").toList.toIndexedSeq)
    val b = summer.wordCount("Times they are a changin Dylan".split(" ").toList.toIndexedSeq)
    val c = summer.wordCount("Times they are a changin".split(" ").toList.toIndexedSeq)

    Par.run(es)(Par.choiceNAsChooser(choice)(List(a, b, c))).get shouldEqual 6
  }

  "A join" should "Flatten the Par" in {
    val es = Executors.newCachedThreadPool()
    val choice = Par.unit(Par.unit(1))
    Par.run(es)(Par.join(choice)).get shouldEqual 1
  }

  "A flatMap" should "apply Map and then Join" in {
    val es = Executors.newCachedThreadPool()
    Par.run(es)(Par.flatMap[String, Int](Par.unit("Hello World"))
      (s => summer.wordCount(s.split(" ").toList.toIndexedSeq))).get shouldEqual 2
  }

}
