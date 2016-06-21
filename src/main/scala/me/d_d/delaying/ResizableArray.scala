package me.d_d.delaying

import me.d_d.delaying.ResizableArray.Update

/** Resizable array supporting prepends and pops (from the head).
  *
  * It stores chunks of powers-of-2 elements, according to the binary representation of n (number of elements).
  * To avoid frequent and costly reshapes, prepended elements are stored in a head cache,
  * and reshapes are only triggered when the head cache is full, 16 elements at the time.
  *
  * ==Summary of complexities==
  *   Apply: O(1) amortized, O(n) worst case
  *   Update: O(lg n), O(n) worst case (if updates are applied)
  *   n appends/prepends: O(n lg n), best case O(1), worst case O(n)
  */
class ResizableArray(val arrays: Array[Array[Int]],
                     val arraysTotalSize: Int,
                     val heads: Array[Int],
                     var updates: Array[Update]) {
  arr =>

  import ResizableArray._

  assert(arraysTotalSize % BLOCK_SIZE == 0)

  val size = heads.length + arraysTotalSize

  /** Dmitry's version
    */
  private def applyArr(idx: Int): Int = {
    val hb = 32 - Integer.numberOfLeadingZeros(idx)
    val mask = (1 << hb) - 1
    val maskedSum = arraysTotalSize & mask

    var arrayId = 0
    var elemId = 0
    if (maskedSum > idx) {
      arrayId = hb - 1
      elemId = idx - (arraysTotalSize & (mask >>> 1))
    } else {
      arrayId = Integer.numberOfTrailingZeros(arraysTotalSize & ~mask)
      elemId = idx - maskedSum
    }

    if (updates ne null) {

      val updatesHead = updates(arrayId)

      if (null ne updatesHead) {
        if (updatesHead.coins >= arrays(arrayId).length) {
          flushUpdates(arrayId, updates(arrayId))
        } else {
          var u = updatesHead
          while (u ne null) {
            updatesHead.coins += 1
            if (u.elemId == elemId) {
              return u.newValue
            }
            u = u.next
          }
        }
      }
    }

    arrays(arrayId)(elemId)
  }

  // Inlineme
  def apply(idx: Int): Int = {
    if (idx < heads.length)
      heads(idx)
    else
      applyArr(idx - heads.length)
  }

  private def flushUpdates(arrayId: Int, updatesHead: Update): Unit = {
    var u = updatesHead
    if (u != null) {
      val mark = new Array[Boolean](arrays(arrayId).length)
      val newArray = arrays(arrayId).clone()
      while (u != null) {
        val elemId = u.elemId
        if (!mark(elemId)) {
          mark(elemId) = true
          newArray(elemId) = u.newValue
        }
        u = u.next
      }
      arrays(arrayId) = newArray
      // BARRIER HERE!!! The order between these two instructions must be strict
      updates(arrayId) = null

      if (updates.forall(null.eq))
        updates = null
    }
  }

  private def updateArr(idx: Int, newElem: Int): ResizableArray = {
    val hb = 32 - Integer.numberOfLeadingZeros(idx)
    val mask = (1 << hb) - 1
    val maskedSum = arraysTotalSize & mask

    var arrayId = 0
    var elemId = 0
    if (maskedSum > idx) {
      arrayId = hb - 1
      elemId = idx - (arraysTotalSize & (mask >>> 1))
    } else {
      arrayId = Integer.numberOfTrailingZeros(arraysTotalSize & ~mask)
      elemId = idx - maskedSum
    }

    val updatesHead = {
      if (null eq updates)
        null
      else
        updates(arrayId)
    }

    val newUpdates = {
      if (null eq updates)
        new Array[Update](arrays.length)
      else
        updates.clone()
    }

    val newCoins = if (null eq updatesHead) 0 else updatesHead.coins + 1

    newUpdates(arrayId) = new Update(elemId, newElem, updatesHead, newCoins)

    new ResizableArray(arrays, arraysTotalSize, heads, newUpdates)
  }

  def updated(idx: Int, newElem:Int): ResizableArray = {
    if (idx < heads.length) {
      val newHeads = heads.clone()
      newHeads(idx) = newElem
      new ResizableArray(arrays, arraysTotalSize, newHeads, updates)
    } else {
      updateArr(idx - heads.length, newElem)
    }
  }

  def prepend(elem: Int): ResizableArray = {
    if (heads.length + 1 < BLOCK_SIZE) {
      val newHeadsSize = heads.length + 1
      val newHeads = new Array[Int](newHeadsSize)
      System.arraycopy(heads, 0, newHeads, 1, heads.length)
      newHeads(0) = elem
      new ResizableArray(arrays, arraysTotalSize, newHeads, updates)
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

        // TODO: Apply updates


        System.arraycopy(arrays(rollingSourceArrayId), 0, target, rollingTargetElemId, arrays(rollingSourceArrayId).length)
        rollingTargetElemId += arrays(rollingSourceArrayId).length
        rollingSourceArrayId += 1
      }

      if (newArrayIdx + 1 < arrays.size)
        System.arraycopy(arrays, newArrayIdx + 1, newArrays, newArrayIdx + 1, arrays.size - newArrayIdx - 1)

      newArrays(newArrayIdx) = target
      new ResizableArray(newArrays, arraysTotalSize + BLOCK_SIZE, Array.empty, null)
    }
  }

  def popHead(): ResizableArray = {
    this
  }

  private def flushAllUpdates(): Unit = {
    if (updates != null) {
      for (i <- 0 until updates.size) {
        if (updates(i) != null)
	  flushUpdates(i, updates(i))
      }
    }
  }

  def foreach(f : Int => Unit): Unit = {
    flushAllUpdates()

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
    flushAllUpdates()

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

  val empty = new ResizableArray(Array.empty, 0, Array.empty, null)

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
    val (heads, chunks) = elems.splitAt(elems.size % BLOCK_SIZE)
    val arrays = createArrays(chunks: _ *)
    new ResizableArray(arrays, chunks.size, heads.toArray, null)
  }

  class Update(val elemId: Int, val newValue: Int, val next: Update, var coins: Int)
}
