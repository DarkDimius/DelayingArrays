import me.d_d.delaying.DArray
import org.scalatest.prop.PropertyChecks

import collection.mutable.Stack
import org.scalatest._

class Spec extends FlatSpec with Matchers with PropertyChecks {

  val sizes = List(1000, 123132, 123132, 1)


  "DArray" should "be safe to iterate" in {
    sizes foreach testIter
  }

  "DArray" should "be safe to iterator.iterate" in {
    sizes foreach testIteratorIter
  }

  "DArray" should "be safe to foreach" in {
    sizes foreach testForeach
  }

  def testIter(size: Int) = {
    val r = 0 until size
    val arr = DArray(r: _*)
    for(x <- r) {
      try {assert(arr.apply(x) == x)}
      catch {case e: Throwable => throw new RuntimeException("failed test size: " + size, e)}
    }
  }

  def testIteratorIter(size: Int) = {
    val r = 0 until size
    val arr = DArray(r: _*)
    val iter = arr.iterator
    val iter2 = r.iterator
    while(iter2.hasNext) {
      assert(iter.hasNext)
      assert(iter.next() == iter2.next())
    }
    assert(!iter.hasNext)
  }

  def testForeach(size: Int) = {
    val r = 0 until size
    val arr = DArray(r: _*)
    var list1: List[Int] = Nil
    var list2: List[Int] = Nil
    r.foreach(x => list1 = x :: list1)
    arr.foreach(x => list2 = x :: list2)
    assert(list1 == list2)
  }



  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    }
  }
}
