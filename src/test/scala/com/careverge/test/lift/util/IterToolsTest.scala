package com.careverge.test.lift.util

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

import com.careverge.lift.util.IterTools.takeDefinedPairValues

/**
 * User: jeffmay
 * Date: 10/19/12
 * Time: 12:57 PM
 */
class IterToolsTest extends FunSpec
  with ShouldMatchers
{

  /*
    Class hierarchy for type testing.

           A
          / \
        B    C
       / \  / \
      D  E F  G
   */

  case class A(x: Int)
  // left side
  case class B(override val x: Int) extends A(x)
  case class D(override val x: Int) extends B(x)
  case class E(override val x: Int) extends B(x)
  // right side
  case class C(override val x: Int) extends A(x)
  case class F(override val x: Int) extends C(x)
  case class G(override val x: Int) extends C(x)

  describe("takeDefinedPairValues") {

    val mixedTypeData = Map(
      D(1) -> Some(F(1)),
      D(2) -> Some(G(2)),
      E(3) -> Some(F(3)),
      E(4) -> Some(G(4)),
      E(5) -> None
    )

    it ("should return mixed types of keys and values as instances of their common ancestors") {
      val actual = takeDefinedPairValues(mixedTypeData).toMap
      val test1: C = actual(D(1))
      test1 should be (F(1))
    }

    it ("should not convert types when there is no implicit converter") {
      val actual = takeDefinedPairValues(mixedTypeData).toMap
      val test1 = actual(D(1))
      test1.isInstanceOf[String] should be (false)
    }

    it ("should convert properly when there is an implicit converter") {
      implicit def cToString: (C => String) = _.x.toString
      val actual = takeDefinedPairValues(mixedTypeData).toMap
      val test1: String = actual(D(1))
      test1 should be ("1")
    }

    it ("should not find values for superclass types") {
      val actual = takeDefinedPairValues(mixedTypeData).toMap
      val test1 = actual.get(B(2))
      test1 should be (None)
    }

    val sampleData: Map[String, Option[Int]] = Map(
      "A" -> Option(1),
      "B" -> None,
      "C" -> Some(2),
      "D" -> None
    )

    it ("should not contain any options not defined in a given iterable") {
      val actual: Iterable[(String, Int)] = takeDefinedPairValues(sampleData)
      for ((key, value) <- actual) {
        val expected = sampleData.get(key)
        assert(expected != None)
      }
    }

    it ("should remove all None options of a given iterable") {
      val actual = takeDefinedPairValues(sampleData)
      for ((key, value) <- actual) {
        val expected = sampleData.get(key)
        assert(expected.get != None)
      }
    }

    it ("should keep all defined options of a given iterable") {
      val actual = takeDefinedPairValues(sampleData)
      for ((key, value) <- actual) {
        val expected = sampleData.get(key)
        assert(expected.get.get == value)
      }
    }

  }
}
