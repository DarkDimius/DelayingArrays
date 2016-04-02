package me.d_d.delaying

import org.scalatest._

import scala.collection.mutable.ArrayBuffer

class DelayingArraySuite extends FunSuite with Matchers {

  val sizes = Array(0, 1, 2, 6, 7, 8, 10, 24, 35, 60, 100, 250, 310, 511, 512, 1025, 12345678)

  test("apply") {
    for (size <- sizes) {
      val range = 0 until size
      val darr = DelayingArray(range: _*)
      assert(darr.size === size)
      for (i <- range)
        assert(i === darr.apply(i))
    }
  }

  test("append") {
    for (size <- sizes) {
      val range = 0 until size
      val darr = range.foldLeft(DelayingArray.empty)(_ append _)
      assert(darr.size === size)
      for (i <- range)
        assert(i === darr.apply(i))
    }
  }

  test("foreach traverses all elements") {
    for (size <- sizes) {
      val range = 0 until size
      val darr = DelayingArray(range: _*)

      var sum = 0L
      darr.foreach(sum += _)
      val targetSum = (0L /: range)(_ + _)

      assert(sum === targetSum)
    }
  }

  test("foreach respects sequential order") {
    for (size <- sizes) {
      val range = 0 until size
      val darr = DelayingArray(range: _*)

      val buf = ArrayBuffer[Int]()
      darr.foreach(buf += _)

      buf should equal (range)
    }
  }

  test("iterator respects sequential order") {
    for (size <- sizes) {
      val range = 0 until size
      val darr = DelayingArray(range: _*)

      val buf = ArrayBuffer[Int]()
      darr.iterator.foreach(buf += _)

      buf should equal (range)
    }
  }
}
