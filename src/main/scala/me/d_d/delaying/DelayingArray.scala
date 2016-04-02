package me.d_d.delaying

class ResizableArray(val arrays: Array[Array[Int]], val arraysTotalSize: Int, val heads: Array[Int]) {

  arr =>

  import ResizableArray._

  //assert(arraysTotalSize % BLOCK_SIZE == 0)

  val size = heads.length + arraysTotalSize

  // Dmitry's version
  def applyArr(idx: Int): Int = {
    val hb = 32 - Integer.numberOfLeadingZeros(idx)
    val mask = (1 << hb) - 1
    val maskedSum = arraysTotalSize & mask
    if (maskedSum > idx) {
      val arrayId = hb - 1
      val elemId = idx - (arraysTotalSize & (mask >>> 1))
      //println(s"-> size=$size idx=$idx arrayId=$arrayId elemId=$elemId")
      arrays(arrayId)(elemId)
    } else {
      val arrayId = Integer.numberOfTrailingZeros(arraysTotalSize & ~mask)
      val elemId = idx - maskedSum
      //println(s"size=$size idx=$idx arrayId=$arrayId elemId=$elemId")
      arrays(arrayId)(elemId)
    }
  }

  def apply(idx: Int): Int = {
    if (idx < heads.length)
      heads(idx)
    else
      applyArr(idx - heads.length)
  }

  def prepend(elem: Int): ResizableArray = {
    if (heads.length + 1 < BLOCK_SIZE) {
      val newHeadsSize = heads.length + 1
      val newHeads = new Array[Int](newHeadsSize)
      System.arraycopy(heads, 0, newHeads, 1, heads.length)
      newHeads(0) = elem
      new ResizableArray(arrays, arraysTotalSize, newHeads)
    } else {
      // 'heads' is full, merge it into 'arrays'
      // 'arraysTotalSize' has the first 'BLOCK_BITS' bits off
      // We need the index of the first 0 bit, after the first 'BLOCK_BITS' bits
      val newArrayIdx = Integer.numberOfTrailingZeros(~(arraysTotalSize | (BLOCK_SIZE - 1)))
      val newArraysSize = arrays.length max (newArrayIdx + 1)
      val newArrays = new Array[Array[Int]](newArraysSize)

      val target = new Array[Int](1 << newArrayIdx)
      target(0) = elem
      System.arraycopy(heads, 0, target, 1, heads.length)

      var rollingSourceArrayId = BLOCK_BITS
      var rollingTargetElemId = BLOCK_SIZE

      while (rollingSourceArrayId < newArrayIdx) {
        System.arraycopy(arrays(rollingSourceArrayId), 0, target, rollingTargetElemId, arrays(rollingSourceArrayId).length)
        rollingTargetElemId += arrays(rollingSourceArrayId).length
        rollingSourceArrayId += 1
      }

      if (newArrayIdx + 1 < arrays.size)
        System.arraycopy(arrays, newArrayIdx + 1, newArrays, newArrayIdx + 1, arrays.size - newArrayIdx - 1)

      newArrays(newArrayIdx) = target

      new ResizableArray(newArrays, arraysTotalSize + BLOCK_SIZE, Array.empty)
    }
  }

  def foreach(f : Int => Unit): Unit = {
    var i = 0
    while (i < heads.length) {
      f(heads(i))
      i += 1
    }

    i = 0
    while (i < arrays.length) {
      if (arrays(i) ne null) {
        var j = 0
        while (j < arrays(i).length) {
          f(arrays(i)(j))
          j += 1
        }
      }
      i += 1
    }
  }

  // reverse foreach
  def rforeach(f : Int => Unit): Unit = {
    var i = arrays.length - 1
    while (i >= 0) {
      if (arrays(i) ne null) {
        var j = arrays(i).length - 1
        while (j >= 0) {
          f(arrays(i)(j))
          j -= 1
        }
      }
      i -= 1
    }

    i = heads.length - 1
    while (i >= 0) {
      f(heads(i))
      i -= 1
    }
  }

  def iterator: Iterator[Int] = new Iterator[Int] {
    private var index = 0
    private var curArr = Integer.numberOfTrailingZeros(arraysTotalSize)
    private var curIndex = 0

    final def hasNext: Boolean = (index < arr.size)

    final def next(): Int = {
      if (!hasNext)
        throw new NoSuchElementException

      val elem = {
        if (index < heads.length)
          heads(index)
        else {
          if (curIndex >= arrays(curArr).length) {
            do curArr += 1 while (arrays(curArr).eq(null))
            curIndex = 0
          }
          val tmp = arrays(curArr)(curIndex)
          curIndex += 1
          tmp
        }
      }
      index += 1
      elem
    }
  }

  // reverse iterator
  def riterator: Iterator[Int] = new Iterator[Int] {
    private var index = arr.size - 1
    private var curArr = arrays.length
    private var curIndex = -1

    final def hasNext: Boolean = (index >= 0)

    final def next(): Int = {
      if (!hasNext)
        throw new NoSuchElementException

      val elem = {
        if (index < heads.length)
          heads(index)
        else {
          if (curIndex < 0) {
            do curArr -= 1 while (arrays(curArr).eq(null))
            curIndex = arrays(curArr).length - 1
          }
          val tmp = arrays(curArr)(curIndex)
          curIndex -= 1
          tmp
        }
      }
      index -= 1
      elem
    }
  }

}

object ResizableArray {
  final val BLOCK_BITS = 4
  final val BLOCK_SIZE = 1 << BLOCK_BITS

  val empty = new ResizableArray(Array.empty, 0, Array.empty)

  def createArrays(elems: Int*): Array[Array[Int]] = {
    val n = elems.size
    val arrays = Array.ofDim[Array[Int]](32 - Integer.numberOfLeadingZeros(n))
    var start = 0
    for {
      i <- arrays.indices
      if (n & (1 << i)) != 0
    } /* do */ {
      arrays(i) = elems.slice(start, start + (1 << i)).toArray
      start += (1 << i)
    }

    arrays
  }

  def apply(elems: Int*): ResizableArray = {
    val (heads, arrays) = elems.splitAt(elems.size % BLOCK_SIZE)
    new ResizableArray(createArrays(arrays: _ *), arrays.size, heads.toArray)
  }
}

class DelayingArray(val left: ResizableArray, val right: ResizableArray) {

  val size: Int = left.size + right.size

  def apply(idx: Int) = {
    if (idx < left.size)
      left(idx)
    else
      right(size - idx - 1)
  }

  def append(elem: Int): DelayingArray = {
    // rebalance?
    new DelayingArray(left, right prepend elem)
  }

  def prepend(elem: Int): DelayingArray = {
    // rebalance?
    new DelayingArray(left prepend elem, right)
  }

  def reverse = new DelayingArray(right, left)

  def foreach(f: Int => Unit): Unit = {
    left.foreach(f)
    right.rforeach(f)
  }

  // Optimize
  def iterator: Iterator[Int] = new Iterator[Int] {
    val leftIt = left.iterator
    val rightIt = right.riterator
    final def hasNext: Boolean = leftIt.hasNext || rightIt.hasNext
    final def next(): Int = {
      if (leftIt.hasNext) leftIt.next()
      else if (rightIt.hasNext) rightIt.next()
      else throw new NoSuchElementException
    }
  }
}

class DelayingArrayIterator(val darr: DelayingArray) { /// extends Iterator[Int] {

  private var index = 0

  private var leftArr = Integer.numberOfTrailingZeros(darr.left.arraysTotalSize)
  private var leftIndex = 0

  private var rightArr = darr.right.arrays.length
  private var rightIndex = -1

  final val leftArrays = darr.left.arrays
  final val rightArrays = darr.right.arrays
  final val leftSize = darr.left.size
  final val leftHeadsSize = darr.left.heads.length

  final def hasNext: Boolean = (index < darr.size)

  def foreach(f: Int => Unit): Unit = {
    while (hasNext)
      f(next())
  }

  final def next(): Int = {
    if (!hasNext) throw new NoSuchElementException

    val elem: Int = {
      // iterate over left
      if (index < leftSize) {
        if (index < leftHeadsSize)
          darr.left.heads(index)
        else {
          if (leftIndex >= leftArrays(leftArr).length) {
            do leftArr += 1 while (leftArrays(leftArr).eq(null))
            leftIndex = 0
          }
          val tmp = leftArrays(leftArr)(leftIndex)
          leftIndex += 1
          tmp
        }
      }
      // iterate over right
      else {
        if (index < darr.left.size + darr.right.arraysTotalSize) {
          if (rightIndex < 0) {
            do rightArr -= 1 while (rightArrays(rightArr).eq(null))
            rightIndex = rightArrays(rightArr).length - 1
          }
          val tmp = rightArrays(rightArr)(rightIndex)
          rightIndex -= 1
          tmp
        } else
          darr.right.heads(darr.size - index - 1)
      }
    }

    index += 1
    elem
  }
}

object DelayingArray {

  val empty = new DelayingArray(ResizableArray.empty, ResizableArray.empty)

  def apply(elems: Int*): DelayingArray = {
    // elems.foldLeft(empty)(_ append _)
    val (left, right) = elems.splitAt(elems.size / 2)
    new DelayingArray(ResizableArray(left: _*), ResizableArray(right.reverse: _*))
  }
}
