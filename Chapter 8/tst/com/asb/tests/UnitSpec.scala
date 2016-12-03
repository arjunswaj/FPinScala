package com.asb.tests

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks

abstract class UnitSpec extends PropSpec with TableDrivenPropertyChecks with Matchers