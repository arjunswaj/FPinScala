package com.asb.pbt

import com.asb.tests.UnitSpec
import org.scalacheck.{Gen, Prop}

/**
  * Property Based Testing.
  * Created by arjun on 03/12/16.
  */
class PropertyBasedTesting extends UnitSpec {


  property("Reverse of reverse is same") {
    val intList = Gen.listOf(Gen.choose(0, 100))
    val prop = Prop.forAll(intList) {
      ns => ns.reverse.reverse == ns
    }
    prop.check
  }

  property("Last of Reverse is same as head") {
    val intList = Gen.listOf(Gen.choose(0, 100))
    val prop = Prop.forAll(intList) {
      ns => ns.headOption == ns.reverse.lastOption
    }
    prop.check
  }

  property("Failing test") {
    val intList = Gen.listOf(Gen.choose(0, 100))
    val prop = Prop.forAll(intList) {
      ns => ns.reverse == ns
    }
    prop.check
  }

  property("Reversing and summing should give same value") {
    val intList: Gen[List[Int]] = Gen.listOf(Gen.choose(0, 100))
    val prop = Prop.forAll(intList) {
      ls => ls.sum == ls.reverse.sum
    }
    prop.check
  }

  property("Sum of elements with same value") {
    val intList = Gen.listOf(Gen.choose(5, 5))
    val prop = Prop.forAll(intList) {
      ls => ls.sum == ls.length * ls.headOption.getOrElse(0)
    }
    prop.check
  }

}
