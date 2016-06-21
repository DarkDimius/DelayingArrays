package me.d_d.delaying

class DelayingArray(val left: ResizableArray, val right: ResizableArray) {

  val size: Int = left.size + right.size

  def apply(idx: Int) = {
    if (idx < left.size)
      left(idx)
    else
      right(size - idx - 1)
  }

  def updated(idx: Int, value: Int): DelayingArray = {
    if (idx < left.size)
      new DelayingArray(left.updated(idx, value), right)
    else
      new DelayingArray(left, right.updated(size - idx - 1, value))
  }

  def append(elem: Int): DelayingArray = {
    new DelayingArray(left, right prepend elem)
  }

  def prepend(elem: Int): DelayingArray = {
    new DelayingArray(left prepend elem, right)
  }

  def reverse = new DelayingArray(right, left)

  def foreach(f: Int => Unit): Unit = {
    left.foreach(f)
    right.rforeach(f)
  }

  // Optimize
  def iterator: DelayingArrayIterator = new DelayingArrayIterator(this)
}

class DelayingArrayIterator(val darr: DelayingArray) {

  private var index = 0

  // First non-null entry
  private var leftArr = Integer.numberOfTrailingZeros(darr.left.arraysTotalSize)
  private var leftIndex = 0

  private var rightArr = darr.right.arrays.length
  private var rightIndex = -1

  private final val leftArrays = darr.left.arrays
  private final val rightArrays = darr.right.arrays
  private final val leftHeads = darr.right.heads
  private final val rightHeads = darr.right.heads

  private final val leftSize = darr.left.size
  private final val leftHeadsSize = darr.left.heads.length
  private final val beforeRightHead = leftSize + darr.right.arraysTotalSize
  private final val darrSize = darr.size

  final def hasNext: Boolean = (index < darr.size)

  final def foreach(f: Int => Unit): Unit = {
    while (hasNext)
      f(next())
  }

  final def leftNext(): Int = {
    if (index >= leftHeadsSize) {
      if (leftIndex >= leftArrays(leftArr).length) {
        do leftArr += 1 while (leftArrays(leftArr).eq(null))
        leftIndex = 0
      }
      val tmp = leftArrays(leftArr)(leftIndex)
      leftIndex += 1
      tmp
    } else
     leftHeads(index)
  }

  final def rightNext(): Int = {
    if (index < beforeRightHead) {
      if (rightIndex < 0) {
        do rightArr -= 1 while (rightArrays(rightArr).eq(null))
        rightIndex = rightArrays(rightArr).length - 1
      }
      val tmp = rightArrays(rightArr)(rightIndex)
      rightIndex -= 1
      tmp
    } else
      rightHeads(darrSize - index - 1)
  }

  final def next(): Int = {
    if (!hasNext) throw new NoSuchElementException
    val elem: Int = {
      if (index < leftSize)
        leftNext()
      else
        rightNext()
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
