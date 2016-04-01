package me.d_d.delaying

class ResizableArray(val arrays: Array[Array[Int]], val size: Int) {

  def applyLoop(idx: Int): Int = {
    var r = idx
    var c = size
    var lb = Integer.lowestOneBit(c)
    while (r >= lb && r != 0) {
      r -= lb
      c -= lb
      lb = Integer.lowestOneBit(c)
    }
    arrays(Integer.numberOfTrailingZeros(c))(r)
  }

  // Dmitry's version
  def apply(idx: Int): Int = {
    val hb = 32 - Integer.numberOfLeadingZeros(idx)
    val mask = (1 << hb) - 1
    val maskedSum = size & mask
    if (maskedSum > idx) {
      val arrayId = hb - 1
      val elemId = idx - (size & (mask >>> 1))
      //println(s"-> size=$size idx=$idx arrayId=$arrayId elemId=$elemId")
      arrays(arrayId)(elemId)
    } else {
      val arrayId = Integer.numberOfTrailingZeros(size & ~mask)
      val elemId = idx - maskedSum
      //println(s"size=$size idx=$idx arrayId=$arrayId elemId=$elemId")
      arrays(arrayId)(elemId)
    }
  }

  // Superscalar no MUL
  def apply_no_MUL(idx: Int): Int = {
    val lb = 31 ^ Integer.numberOfLeadingZeros(idx)
    val mask = (1 << (lb+1)) - 1
    val t = Integer.numberOfTrailingZeros(size & ~mask)
    val maskedSum = size & mask
    val corr = (idx - maskedSum) >>> 31
    arrays(t - ((t - lb) & -corr))(idx - (maskedSum ^ (corr << lb)))
  }

  // Superscalar 1
  def applySuperscalar1(idx: Int): Int = {
    //println(s"size = $size idx = $idx")
    val lb1 = 32 - Integer.numberOfLeadingZeros(idx)
    val lb = lb1 - 1
    val mask = (1 << lb1) - 1
    val t = Integer.numberOfTrailingZeros(size & ~mask)
    val maskedSum = size & mask
    val corr = (idx - maskedSum) >>> 31
    arrays(t - ((t - lb) & (-corr))) (idx - (maskedSum ^ (corr << lb)))
  }

  // Superscalar SHIFT
  def applySHIFT(idx: Int): Int = {
    //println(s"size = $size idx = $idx")
    val lb = 31 ^ Integer.numberOfLeadingZeros(idx)
    val mask = (1 << (lb+1)) - 1
    val maskedSum = size & mask
    val corr = maskedSum ^ (((idx - maskedSum) >>> 31) << lb)
    arrays(Integer.numberOfTrailingZeros(size ^ corr))(idx - corr)
  }

  // SWAR
  def applySWAR(idx: Int): Int = {
    //println(s"size = $size idx = $idx")
    var mask = (idx | (idx >>> 1))
    mask |= mask >>> 2
    mask |= mask >>> 4
    mask |= mask >>> 8
    mask |= mask >>> 16
    val hb = (mask + 1) >>> 1
    val maskedSum = size & mask
    val corr = hb * ((idx - maskedSum) >>> 31)
    arrays(Integer.numberOfTrailingZeros((size & ~mask) ^ corr))(idx - (maskedSum ^ corr))
  }

  // SWAR no MUL
  def applySWAR_no_MUL(idx: Int): Int = {
    //println(s"size = $size idx = $idx")
    var mask = idx
    mask |= mask >>> 1
    mask |= mask >>> 2
    mask |= mask >>> 4
    mask |= mask >>> 8
    mask |= mask >>> 16
    val hb = (mask + 1) >>> 1
    val maskedSum = size & mask
    //val corr = hb * ((idx - maskedSum) >>> 31)
    val corr = (-((idx - maskedSum) >>> 31) & hb) ^ maskedSum
    arrays(Integer.numberOfTrailingZeros(size ^ corr))(idx - corr)
  }

  // Superscalar 4
  def applySuperscalar(idx: Int): Int = {
    //println(s"size = $size idx = $idx")
    val lb = 31 ^ Integer.numberOfLeadingZeros(idx)//clz
    val mask = (1 << (lb+1)) - 1
    val t = Integer.numberOfTrailingZeros(size & ~mask)
    //val t = Integer.numberOfTrailingZeros(size >>> (lb+1)) + (lb+1)
    val maskedSum = size & mask
    //val correction = maskedSum ^ (((idx - maskedSum) >>> 31) << lb)
    //val corr = ((idx - maskedSum) >>> 31) << lb
    //val correction = maskedSum ^ (((idx - maskedSum) >>> 31) << lb)
    //val arrayId =
    //val elemId =
    //println(s"arrayId=$arrayId elemId=$elemId mask=$mask lb=$lb")
    //val tmp = Integer.numberOfTrailingZeros(size ^ maskedSum ^ corr)
    //if (((idx - maskedSum) >>> 31) != 0) assert(tmp == lb)
    //else assert(tmp == t)
    val corr = ((idx - maskedSum) >>> 31)
    //assert(tmp == answer)
    //arrays(Integer.numberOfTrailingZeros((size & ~mask) ^ corr))(idx - (maskedSum ^ corr))
    //val answer = t - (t - lb) * corr
    // val target = Integer.numberOfTrailingZeros((size & ~mask) ^ (corr << lb))

    //assert(answer == target)

    arrays(t - (t - lb) * corr)(idx - (maskedSum ^ (corr << lb)))
    //arrays((t - (t+lb) * corr))(idx - (maskedSum ^ (corr << lb)))
    //if (maskedSum <= idx) {
    //val elemId = (idx - maskedSum)
    //arrays(arrayId)(elemId)
    //} else {
    //val arrayId = hb - 1
    //val elemId = idx - (size & (mask >>> 1))
    //println(s"arrayId=$arrayId elemId=$elemId mask=$mask")
    //arrays(arrayId)(elemId)
    //}
  }

  def prepend(elem: Int): ResizableArray = {
    val newArrayIdx = Integer.numberOfTrailingZeros(~size)
    val newArraysSize = arrays.length max (newArrayIdx + 1)
    val newArrays = new Array[Array[Int]](newArraysSize)

    var rollingSourceArrayId = 0
    var rollingTargetElemId = 1
    val target = new Array[Int](1 << newArrayIdx)

    while (rollingSourceArrayId < newArrayIdx) {
      System.arraycopy(arrays(rollingSourceArrayId), 0, target, rollingTargetElemId, arrays(rollingSourceArrayId).length)
      rollingTargetElemId += arrays(rollingSourceArrayId).length
      rollingSourceArrayId += 1
    }

    if (newArrayIdx + 1 < arrays.size)
      System.arraycopy(arrays, newArrayIdx + 1, newArrays, newArrayIdx + 1, arrays.size - newArrayIdx - 1)

    target(0) = elem
    newArrays(newArrayIdx) = target

    new ResizableArray(newArrays, size + 1)
  }

  def foreach(f : Int => Unit): Unit = {
    var i = 0
    while (i < arrays.length) {
      if (arrays(i) ne null) {
        var j = 0
        while (j < arrays.length) {
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
  }

}

object ResizableArray {

  val empty = new ResizableArray(Array.empty, 0)

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
    new ResizableArray(createArrays(elems: _ *), elems.size)
  }
}

class DelayingArray(left: ResizableArray, right: ResizableArray) {
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

  // TODO: Optimize
  def iterator: Iterator[Int] = new Iterator[Int] {
    private var index = 0
    def hasNext: Boolean = (index < size)
    def next(): Int = {
      if (!hasNext) throw new NoSuchElementException
      val elem = apply(index)
      index += 1
      elem
    }
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
