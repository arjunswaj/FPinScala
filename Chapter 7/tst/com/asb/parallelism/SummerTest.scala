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

  "A Summer" should "sum in divide and conquer as well" in {
    summer.sumDQ(Seq(1, 2, 3, 4, 5)) shouldEqual 15
  }

  it should "be same as original summer" in {
    val seq = Seq(1, 2, 3, 4, 5)
    summer.sum(seq) should equal (summer.sumDQ(seq))
  }

}
