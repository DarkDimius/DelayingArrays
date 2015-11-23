import me.d_d.delaying.DArray
import org.scalatest.prop.PropertyChecks

import collection.mutable.Stack
import org.scalatest._

class Spec extends FlatSpec with Matchers with PropertyChecks {

  def testSize(size: Int) = {
    val r = 0 until size
    val arr = DArray(r: _*)
    for(x <- r) {
      try {assert(arr.apply(x) == x)}
      catch {case e: Throwable => throw new RuntimeException("failed test size: " + size, e)}
    }
  }


  "DArray-1000" should "be safe to iterate" in {
    testSize(1000)
  }

  "DArray-123132" should "be safe to iterate" in {
    testSize(123132)
  }

  "DArray-7" should "be safe to iterate" in {
    testSize(123132)
  }
  "DArray-1" should "be safe to iterate" in {
    testSize(1)
  }


  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    }
  }
}
