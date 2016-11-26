package com.asb.parallelism

import java.util.concurrent.Executors

import com.asb.tests.UnitSpec

/**
  * Non Blocking Test.
  * Created by arjun on 26/11/16.
  */
class NonBlockingTest extends UnitSpec {

  "A choice" should "give one Par" in {
    val es = Executors.newFixedThreadPool(1)
    val cond = NonBlocking.Par.unit(true)
    val a = summer.nonBlockingWordCount("Times they are a changin Dylan".split(" ").toList.toIndexedSeq)
    val b = summer.nonBlockingWordCount("Times they are a changin".split(" ").toList.toIndexedSeq)
    NonBlocking.Par.run(es)(NonBlocking.Par.choice(cond)(a, b)) shouldEqual 6
  }

}
