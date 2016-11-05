package com.asb.parallelism

import com.asb.tests.UnitSpec

/**
  * Summer Test.
  * Created by arjun on 05/11/16.
  */
class SummerTest extends UnitSpec {

  "A Summer" should "sum the values" in {
    summer.sum(Seq(1, 2, 3, 4, 5)) shouldEqual 15
  }

}
