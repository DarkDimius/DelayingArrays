package me.d_d.delaying

/** Companion object to the RRBVector class
  */
object IntRRBVector {
  def newBuilder: IntRRBVectorBuilder = new IntRRBVectorBuilder()

  def apply(elems: Int*) = {
    val b = newBuilder
    for (elem <- elems) {
      b += elem
    }
    b.result()
  }

  def applyr(elems: Int*) = {
    var t = empty
    for(elem <- elems) {
      t = t :+ elem
    }
    t
  }

  def applyl(elems: Int*) = {
    var t = empty
    for(elem <- elems.reverse) {
      t = elem +: t
    }
    t
  }

  lazy private val EMPTY_VECTOR = new IntRRBVector(0)

  def empty: IntRRBVector = EMPTY_VECTOR

  private[delaying] final val emptyTransientBlock = new Array[AnyRef](2)
}

// in principle, most members should be private. however, access privileges must
// be carefully chosen to not prevent method inlining

/** IntRRBVector is a general-purpose, immutable data structure.  It provides random access, updates, concatenations, insertions and splits
  * in effectively constant time, as well as amortized constant time append, prepend and local updates.  It is backed by a little
  * endian bit-mapped vector trie with a branching factor of 32.  Locality is very good, but not
  * contiguous, which is good for very large sequences.
  */
final class IntRRBVector private[delaying](override private[delaying] val endIndex: Int) extends IntRRBVectorPointer {
  self =>

  private[delaying] var transient: Boolean = false

  def length: Int = endIndex

  def lengthCompare(len: Int): Int = endIndex - len

  def foreach(f: Int => Unit): Unit = {
    // TODO make efficient implementation
    var i = 0
    while (i < length) {
      f(apply(i))
      i += 1
    }
  }

  def iterator: IntRRBVectorIterator = {
    if (this.transient) {
      this.normalize(depth)
      this.transient = false
    }
    val it = new IntRRBVectorIterator(0, endIndex)
    it.initIteratorFrom(this)
    it
  }

  def reverseIterator: IntRRBVectorReverseIterator = {
    if (this.transient) {
      this.normalize(depth)
      this.transient = false
    }
    val it = new IntRRBVectorReverseIterator(0, endIndex)
    it.initIteratorFrom(this)
    it
  }


  def apply(index: Int): Int = {
    // keep method size under 35 bytes, so that it can be JIT-inlined

    def getElemFromInsideFocus(index: Int, _focusStart: Int): Int = {
      // extracted to keep method size under 35 bytes, so that it can be JIT-inlined
      val indexInFocus = index - _focusStart
      getElem(indexInFocus, indexInFocus ^ focus)
    }

    def getElemFromOutsideFocus(index: Int): Int = {
      // extracted to keep method size under 35 bytes, so that it can be JIT-inlined
      if /* index is in the vector bounds */ (0 <= index && index < endIndex) {
        if (transient) {
          normalize(depth)
          transient = false
        }
        return getElementFromRoot(index)
      } else
        throw new IndexOutOfBoundsException(index.toString)
    }

    val _focusStart = focusStart
    if /* index is in focused subtree */ (_focusStart <= index && index < focusEnd) {
      return getElemFromInsideFocus(index, _focusStart)
    } else {
      return getElemFromOutsideFocus(index)
    }
  }

  // SeqLike api

  def updated(index: Int, elem: Int): IntRRBVector = {
    val vec = new IntRRBVector(endIndex)
    vec.transient = this.transient
    vec.initWithFocusFrom(this)
    if (index < focusStart || focusEnd <= index || ((index - focusStart) & ~31) != (focus & ~31)) {
      if (index < 0 || endIndex <= index) throw new IndexOutOfBoundsException(index.toString)
      vec.normalizeAndFocusOn(index)
    }
    vec.makeTransientIfNeeded()
    val d0 = copyOf(vec.display0)
    d0((index - vec.focusStart) & 31) = elem
    vec.display0 = d0
    vec
  }

  private def createSingletonVector(elem: Int): IntRRBVector = {
    val resultVector = new IntRRBVector(1)
    resultVector.initSingleton(elem)
    resultVector
  }

  def :+(elem: Int): IntRRBVector = {
    val _endIndex = this.endIndex
    if (_endIndex != 0) {
      val resultVector = new IntRRBVector(_endIndex + 1)
      resultVector.transient = this.transient
      resultVector.initWithFocusFrom(this)
      resultVector.append(elem, _endIndex)
      return resultVector
    } else {
      return createSingletonVector(elem)
    }
  }

  private[delaying] def append(elem: Int, _endIndex: Int): Unit = {
    // keep method size under 35 bytes, so that it can be JIT-inlined
    focusOnLastBlock(_endIndex) // extracted to keep method size under 35 bytes, so that it can be JIT-inlined

    val elemIndexInBlock = (_endIndex - focusStart) & 31
    if /* if next element will go in current block position */ (elemIndexInBlock != 0) {
      appendOnCurrentBlock(elem, elemIndexInBlock)
      return
    } else /* next element will go in a new block position */ {
      appendBackNewBlock(elem, _endIndex)
      return
    }

  }

  // semi-private api

  private final def focusOnFirstBlock(): Unit = {
    // keep method size under 35 bytes, so that it can be JIT-inlined
    if (focusStart != 0 || (focus & -32) != 0) {
      /* the current focused block is not on the left most leaf block of the vector */
      normalizeAndFocusOn(0)
    }
  }

  private final def focusOnLastBlock(_endIndex: Int): Unit = {
    // keep method size under 35 bytes, so that it can be JIT-inlined
    if /* vector focus is not focused block of the last element */ (((focusStart + focus) ^ (_endIndex - 1)) >= 32) {
      normalizeAndFocusOn(_endIndex - 1)
    }
  }

  private final def appendOnCurrentBlock(elem: Int, elemIndexInBlock: Int): Unit = {
    // keep method size under 35 bytes, so that it can be JIT-inlined
    focusEnd = endIndex
    val d0 = copyOf(display0, elemIndexInBlock, elemIndexInBlock + 1)
    d0(elemIndexInBlock) = elem
    display0 = d0
    makeTransientIfNeeded()
  }

  private final def appendBackNewBlock(elem: Int, _endIndex: Int) = {
    val oldDepth = depth
    val newRelaxedIndex = _endIndex - focusStart + focusRelax
    val focusJoined = focus | focusRelax
    val xor = newRelaxedIndex ^ focusJoined
    val _transient = transient
    setupNewBlockInNextBranch(xor, _transient)
    if /* setupNewBlockInNextBranch(...) increased the depth of the tree */ (oldDepth == depth) {
      var i = if (xor < 1024) 2 else if (xor < 32768) 3 else if (xor < 1048576) 4 else if (xor < 33554432) 5 else 6
      if (i < oldDepth) {
        val _focusDepth = focusDepth
        var display: Array[AnyRef] = i match {
          case 2 => display2
          case 3 => display3
          case 4 => display4
          case 5 => display5
        }
        do {
          val displayLen = display.length - 1
          val oldSizes = display(displayLen)
          val newSizes: Array[Int] =
            if (i >= _focusDepth && oldSizes != null) {
              makeTransientSizes(oldSizes.asInstanceOf[Array[Int]], displayLen - 1)
            } else null

          val newDisplay = new Array[AnyRef](display.length)
          System.arraycopy(display, 0, newDisplay, 0, displayLen - 1)
          if (i >= _focusDepth)
            newDisplay(displayLen) = newSizes

          i match {
            case 2 =>
              display2 = newDisplay
              display = display3
            case 3 =>
              display3 = newDisplay
              display = display4
            case 4 =>
              display4 = newDisplay
              display = display5
            case 5 =>
              display5 = newDisplay
          }
          i += 1
        } while (i < oldDepth)
      }
    }

    if (oldDepth == focusDepth)
      initFocus(_endIndex, 0, _endIndex + 1, depth, 0)
    else
      initFocus(0, _endIndex, _endIndex + 1, 1, newRelaxedIndex & -32)

    display0(0) = elem
    transient = true
  }

  private final def makeTransientIfNeeded() = {
    val _depth = depth
    if (_depth > 1 && !transient) {
      copyDisplaysAndNullFocusedBranch(_depth, focus | focusRelax)
      transient = true
    }
  }

  private[delaying] final def normalizeAndFocusOn(index: Int): Unit = {
    if (transient) {
      normalize(depth)
      transient = false
    }
    focusOn(index)
  }

  final def +:(elem: Int): IntRRBVector = {
    val _endIndex = this.endIndex
    if (_endIndex != 0) {
      val resultVector = new IntRRBVector(_endIndex + 1)
      resultVector.transient = this.transient
      resultVector.initWithFocusFrom(this)
      resultVector.prepend(elem)
      return resultVector
    } else {
      return createSingletonVector(elem)
    }
  }

  private[delaying] final def prepend(elem: Int): Unit = {
    // keep method size under 35 bytes, so that it can be JIT-inlined
    focusOnFirstBlock()
    val d0 = display0
    if /* element fits in current block */ (d0.length < 32) {
      prependOnCurrentBlock(elem, d0)
      return
    } else {
      prependFrontNewBlock(elem)
      return
    }
  }

  private final def prependOnCurrentBlock(elem: Int, oldD0: Array[Int]): Unit = {
    // keep method size under 35 bytes, so that it can be JIT-inlined
    def shiftedCopyOf(array: Array[Int], newLen: Int) = {
      // extracted to keep method size under 35 bytes, so that it can be JIT-inlined
      val newArray = new Array[Int](newLen)
      System.arraycopy(array, 0, newArray, 1, newLen - 1)
      newArray
    }
    val newLen = oldD0.length + 1
    focusEnd = newLen
    val newD0 = shiftedCopyOf(oldD0, newLen)
    newD0(0) = elem
    display0 = newD0
    makeTransientIfNeeded()
  }

  private[delaying] final def prependFrontNewBlock(elem: Int): Unit = {
    var currentDepth = focusDepth
    if (currentDepth == 1)
      currentDepth += 1
    var display = currentDepth match {
      case 1 =>
        currentDepth = 2
        display1
      case 2 => display1
      case 3 => display2
      case 4 => display3
      case 5 => display4
      case 6 => display5
    }
    while /* the insertion depth has not been found */ (display != null && display.length == 33) {
      currentDepth += 1
      currentDepth match {
        case 2 => display = display1
        case 3 => display = display2
        case 4 => display = display3
        case 5 => display = display4
        case 6 => display = display5
        case _ => throw new IllegalStateException()
      }
    }

    val oldDepth = depth
    val _transient = transient

    // create new node at this depth and all singleton nodes under it on left most branch
    setupNewBlockInInitBranch(currentDepth, _transient)

    // update sizes of nodes above the insertion depth
    if /* setupNewBlockInNextBranch(...) increased the depth of the tree */ (oldDepth == depth) {
      var i = currentDepth
      if (i < oldDepth) {
        val _focusDepth = focusDepth
        var display: Array[AnyRef] = i match {
          case 2 => display2
          case 3 => display3
          case 4 => display4
          case 5 => display5
        }
        do {
          val displayLen = display.length - 1
          val newSizes: Array[Int] =
            if (i >= _focusDepth) {
              makeTransientSizes(display(displayLen).asInstanceOf[Array[Int]], 1)
            } else null

          val newDisplay = new Array[AnyRef](display.length)
          System.arraycopy(display, 1, newDisplay, 1, displayLen - 1)
          if (i >= _focusDepth)
            newDisplay(displayLen) = newSizes

          i match {
            case 2 =>
              display2 = newDisplay
              display = display3
            case 3 =>
              display3 = newDisplay
              display = display4
            case 4 =>
              display4 = newDisplay
              display = display5
            case 5 =>
              display5 = newDisplay
          }
          i += 1
        } while (i < oldDepth)
      }
    }

    initFocus(0, 0, 1, 1, 0)

    display0(0) = elem
    transient = true
  }

  def isEmpty: Boolean = this.endIndex == 0

  def head: Int = {
    // keep method size under 35 bytes, so that it can be JIT-inlined
    if (this.endIndex != 0)
      return apply(0)
    else
      throw new UnsupportedOperationException("empty.head")
  }

  def take(n: Int): IntRRBVector = {
    // keep method size under 35 bytes, so that it can be JIT-inlined
    if (0 < n) {
      if (n < endIndex)
        return takeFront0(n)
      else
        return this
    } else {
      return IntRRBVector.empty
    }
  }

  def takeRight(n: Int): IntRRBVector = {
    // keep method size under 35 bytes, so that it can be JIT-inlined
    if (0 < n) {
      val _endIndex = endIndex
      if (n < _endIndex) return dropFront0(_endIndex - n)
      else return this
    } else {
      return IntRRBVector.empty
    }
  }

  def drop(n: Int): IntRRBVector = {
    // keep method size under 35 bytes, so that it can be JIT-inlined
    if (n <= 0)
      return this
    else if (n < endIndex)
      return dropFront0(n)
    else
      return IntRRBVector.empty
  }

  def dropRight(n: Int): IntRRBVector = {
    // keep method size under 35 bytes, so that it can be JIT-inlined
    if (n <= 0)
      return this
    else {
      val _endIndex = endIndex
      if (n < _endIndex)
        return takeFront0(_endIndex - n)
      else
        return IntRRBVector.empty
    }
  }

  def slice(from: Int, until: Int): IntRRBVector = take(until).drop(from)

  def splitAt(n: Int): (IntRRBVector, IntRRBVector) = (take(n), drop(n))

  def ++(that: IntRRBVector): IntRRBVector = {
    if (that.isEmpty)
      return this
    else {
      if (this.isEmpty) {
        return that
      } else {
        val thisVecLen = this.endIndex
        val thatVecLen = that.endIndex
        val newVec = new IntRRBVector(thisVecLen + thatVecLen)
        newVec.initWithFocusFrom(this)
        newVec.transient = this.transient
        if (1024 < thisVecLen && thatVecLen <= 32) {
          /* appending a small amount of elements to a large vector */
          newVec.appendAll(thisVecLen, that.display0)
        } else {
          newVec.concatenate(thisVecLen, that)
        }
        return newVec
      }
    }
  }

  def tail: IntRRBVector = {
    // keep method size under 35 bytes, so that it can be JIT-inlined
    if (this.endIndex.!=(0))
      return this.drop(1)
    else
      throw new UnsupportedOperationException("empty.tail")
  }

  def last: Int = {
    // keep method size under 35 bytes, so that it can be JIT-inlined
    if (this.endIndex != 0)
      return this.apply(this.endIndex - 1)
    else
      throw new UnsupportedOperationException("empty.last")
  }

  def init: IntRRBVector = {
    // keep method size under 35 bytes, so that it can be JIT-inlined
    if (this.endIndex != 0)
      return dropRight(1)
    else
      throw new UnsupportedOperationException("empty.init")
  }

  private[delaying] final def appendAll(currentEndIndex: Int, that: Array[Int]): Unit = {
    var _endIndex = currentEndIndex
    val newEndIndex = this.endIndex

    focusOnLastBlock(_endIndex)
    makeTransientIfNeeded()

    var i = 0
    val arrLen = that.length
    while (_endIndex < newEndIndex) {
      val elemIndexInBlock = (_endIndex - focusStart) & 31
      if /* if next element will go in current block position */ (elemIndexInBlock != 0) {
        val batchSize = math.min(32 - elemIndexInBlock, arrLen - i)
        val d0 = new Array[Int](elemIndexInBlock + batchSize)
        System.arraycopy(display0, 0, d0, 0, elemIndexInBlock)
        System.arraycopy(that, i, d0, elemIndexInBlock, batchSize)
        display0 = d0
        _endIndex += batchSize
        focusEnd = _endIndex
        i += batchSize
      } else /* next element will go in a new block position */ {
        appendBackNewBlock(that(i), _endIndex)
        _endIndex += 1
        i += 1
      }
    }
  }

  private[delaying] final def concatenate(currentSize: Int, that: IntRRBVector): scala.Unit = {
    val thisDepth = this.depth
    val thatDepth = that.depth
    if (this.transient) {
      this.normalize(thisDepth)
      this.transient = false
    }

    if (that.transient) {
      that.normalize(thatDepth)
      that.transient = false
    }

    this.focusOn(currentSize - 1)
    math.max(thisDepth, thatDepth) match {
      case 1 =>
        if (endIndex <= 32) {
          val concat = rebalancedLeafs0(display0, that.display0)
          initFromLeaf(concat)
        } else {
          val concat = rebalancedLeafs(display0, that.display0, isTop = true)
          initFromRoot(concat, 2)
        }
        return
      case 2 =>
        var d0: Array[Int] = null
        var d1: Array[AnyRef] = null
        if (((that.focus | that.focusRelax) & -32) == 0) {
          d1 = that.display1
          d0 = that.display0
        } else {
          if (that.display1 != null) d1 = that.display1
          if (d1 == null) d0 = that.display0 else d0 = d1(0).asInstanceOf[Array[Int]]
        }
        var concat: Array[AnyRef] = rebalancedLeafs(this.display0, d0, isTop = false)
        concat = rebalanced(this.display1, concat, that.display1, 2)
        if (concat.length == 2) {
          initFromRoot(concat(0).asInstanceOf[Array[AnyRef]], 2)
          return
        } else {
          initFromRoot(withComputedSizes(concat, 3), 3)
          return
        }
      case 3 =>
        var d0: Array[Int] = null
        var d1: Array[AnyRef] = null
        var d2: Array[AnyRef] = null
        if (((that.focus | that.focusRelax) & -32) == 0) {
          d2 = that.display2
          d1 = that.display1
          d0 = that.display0
        }
        else {
          if (that.display2 != null) d2 = that.display2
          if (d2 == null) d1 = that.display1 else d1 = d2(0).asInstanceOf[Array[AnyRef]]
          if (d1 == null) d0 = that.display0 else d0 = d1(0).asInstanceOf[Array[Int]]
        }
        var concat: Array[AnyRef] = rebalancedLeafs(this.display0, d0, isTop = false)
        concat = rebalanced(this.display1, concat, d1, 2)
        concat = rebalanced(this.display2, concat, that.display2, 3)
        if (concat.length == 2) {
          initFromRoot(concat(0).asInstanceOf[Array[AnyRef]], 3)
          return
        } else {
          initFromRoot(withComputedSizes(concat, 4), 4)
          return
        }
      case 4 =>
        var d0: Array[Int] = null
        var d1: Array[AnyRef] = null
        var d2: Array[AnyRef] = null
        var d3: Array[AnyRef] = null
        if (((that.focus | that.focusRelax) & -32) == 0) {
          d3 = that.display3
          d2 = that.display2
          d1 = that.display1
          d0 = that.display0
        } else {
          if (that.display3 != null) d3 = that.display3
          if (d3 == null) d2 = that.display2 else d2 = d3(0).asInstanceOf[Array[AnyRef]]
          if (d2 == null) d1 = that.display1 else d1 = d2(0).asInstanceOf[Array[AnyRef]]
          if (d1 == null) d0 = that.display0 else d0 = d1(0).asInstanceOf[Array[Int]]
        }
        var concat: Array[AnyRef] = rebalancedLeafs(this.display0, d0, isTop = false)
        concat = rebalanced(this.display1, concat, d1, 2)
        concat = rebalanced(this.display2, concat, d2, 3)
        concat = rebalanced(this.display3, concat, that.display3, 4)
        if (concat.length == 2) {
          initFromRoot(concat(0).asInstanceOf[Array[AnyRef]], 4)
          return
        } else {
          initFromRoot(withComputedSizes(concat, 5), 5)
          return
        }
      case 5 =>
        var d0: Array[Int] = null
        var d1: Array[AnyRef] = null
        var d2: Array[AnyRef] = null
        var d3: Array[AnyRef] = null
        var d4: Array[AnyRef] = null
        if (((that.focus | that.focusRelax) & -32) == 0) {
          d4 = that.display4
          d3 = that.display3
          d2 = that.display2
          d1 = that.display1
          d0 = that.display0
        }
        else {
          if (that.display4 != null) d4 = that.display4
          if (d4 == null) d3 = that.display3 else d3 = d4(0).asInstanceOf[Array[AnyRef]]
          if (d3 == null) d2 = that.display2 else d2 = d3(0).asInstanceOf[Array[AnyRef]]
          if (d2 == null) d1 = that.display1 else d1 = d2(0).asInstanceOf[Array[AnyRef]]
          if (d1 == null) d0 = that.display0 else d0 = d1(0).asInstanceOf[Array[Int]]
        }
        var concat: Array[AnyRef] = rebalancedLeafs(this.display0, d0, isTop = false)
        concat = rebalanced(this.display1, concat, d1, 2)
        concat = rebalanced(this.display2, concat, d2, 3)
        concat = rebalanced(this.display3, concat, d3, 4)
        concat = rebalanced(this.display4, concat, that.display4, 5)
        if (concat.length == 2) {
          initFromRoot(concat(0).asInstanceOf[Array[AnyRef]], 5)
          return
        } else {
          initFromRoot(withComputedSizes(concat, 6), 6)
          return
        }
      case 6 =>
        var d0: Array[Int] = null
        var d1: Array[AnyRef] = null
        var d2: Array[AnyRef] = null
        var d3: Array[AnyRef] = null
        var d4: Array[AnyRef] = null
        var d5: Array[AnyRef] = null
        if ((that.focus & -32) == 0) {
          d5 = that.display5
          d4 = that.display4
          d3 = that.display3
          d2 = that.display2
          d1 = that.display1
          d0 = that.display0
        } else {
          if (that.display5 != null) d5 = that.display5
          if (d5 == null) d4 = that.display4 else d4 = d5(0).asInstanceOf[Array[AnyRef]]
          if (d4 == null) d3 = that.display3 else d3 = d4(0).asInstanceOf[Array[AnyRef]]
          if (d3 == null) d2 = that.display2 else d2 = d3(0).asInstanceOf[Array[AnyRef]]
          if (d2 == null) d1 = that.display1 else d1 = d2(0).asInstanceOf[Array[AnyRef]]
          if (d1 == null) d0 = that.display0 else d0 = d1(0).asInstanceOf[Array[Int]]
        }
        var concat: Array[AnyRef] = rebalancedLeafs(this.display0, d0, isTop = false)
        concat = rebalanced(this.display1, concat, d1, 2)
        concat = rebalanced(this.display2, concat, d2, 3)
        concat = rebalanced(this.display3, concat, d3, 4)
        concat = rebalanced(this.display4, concat, d4, 5)
        concat = rebalanced(this.display5, concat, that.display5, 6)
        if (concat.length == 2) {
          initFromRoot(concat(0).asInstanceOf[Array[AnyRef]], 6)
          return
        } else {
          initFromRoot(withComputedSizes(concat, 7), 7)
          return
        }
      case _ => throw new IllegalStateException()
    }

  }

  private def rebalanced(displayLeft: Array[AnyRef], concat: Array[AnyRef], displayRight: Array[AnyRef], currentDepth: Int): Array[AnyRef] = {
    val leftLength = if (displayLeft == null) 0 else displayLeft.length - 1
    val concatLength = if (concat == null) 0 else concat.length - 1
    val rightLength = if (displayRight == null) 0 else displayRight.length - 1
    val branching = computeBranching(displayLeft, concat, displayRight, currentDepth)
    val top = new Array[AnyRef]((branching >> 10) + (if ((branching & 1023) == 0) 1 else 2))
    var mid = new Array[AnyRef](if ((branching >> 10) == 0) ((branching + 31) >> 5) + 1 else 33)
    var bot: Array[AnyRef] = null
    var iSizes = 0
    var iTop = 0
    var iMid = 0
    var iBot = 0
    var i = 0
    var j = 0
    var d = 0
    var currentDisplay: Array[AnyRef] = null
    var displayEnd = 0
    do {
      d match {
        case 0 =>
          if (displayLeft != null) {
            currentDisplay = displayLeft
            displayEnd = if (concat == null) leftLength else leftLength - 1
          }
        case 1 =>
          if (concat == null)
            displayEnd = 0
          else {
            currentDisplay = concat
            displayEnd = concatLength
          }
          i = 0
        case 2 =>
          if (displayRight != null) {
            currentDisplay = displayRight
            displayEnd = rightLength
            i = if (concat == null) 0 else 1
          }
      }
      while (i < displayEnd) {
        val displayValue = currentDisplay(i).asInstanceOf[Array[AnyRef]]
        val displayValueEnd = if (currentDepth == 2) displayValue.length else displayValue.length - 1
        if /* the current block in displayValue can be used directly (no copies) */ ((iBot | j) == 0 && displayValueEnd == 32) {
          if (currentDepth != 2 && bot != null) {
            withComputedSizes(bot, currentDepth - 1)
            bot = null
          }
          mid(iMid) = displayValue
          i += 1
          iMid += 1
          iSizes += 1
        } else {
          val numElementsToCopy = java.lang.Math.min(displayValueEnd - j, 32 - iBot)
          if (iBot == 0) {
            if (currentDepth != 2 && bot != null)
              withComputedSizes(bot, currentDepth - 1)
            bot = new Array[AnyRef](java.lang.Math.min(branching - (iTop << 10) - (iMid << 5), 32) + (if (currentDepth == 2) 0 else 1))
            mid(iMid) = bot
          }

          System.arraycopy(displayValue, j, bot, iBot, numElementsToCopy)
          j += numElementsToCopy
          iBot += numElementsToCopy
          if (j == displayValueEnd) {
            i += 1
            j = 0
          }

          if (iBot == 32) {
            iMid += 1
            iBot = 0
            iSizes += 1
            if (currentDepth != 2 && bot != null)
              withComputedSizes(bot, currentDepth - 1)
          }

        }
        if (iMid == 32) {
          top(iTop) = if (currentDepth == 1) withComputedSizes1(mid) else withComputedSizes(mid, currentDepth)
          iTop += 1
          iMid = 0
          val remainingBranches = branching - ((iTop << 10) | (iMid << 5) | iBot)
          if (remainingBranches > 0)
            mid = new Array[AnyRef](if ((remainingBranches >> 10) == 0) (remainingBranches + 63) >> 5 else 33)
          else
            mid = null
        }

      }
      d += 1
    }
    while (d < 3)
    if (currentDepth != 2 && bot != null)
      withComputedSizes(bot, currentDepth - 1)

    if (mid != null)
      top(iTop) = if (currentDepth == 1) withComputedSizes1(mid) else withComputedSizes(mid, currentDepth)

    top
  }

  private final def rebalancedLeafs0(displayLeft: Array[Int], displayRight: Array[Int]): Array[Int] = {
    val leftLength = displayLeft.length
    val rightLength = displayRight.length
    val mergedDisplay = new Array[Int](leftLength + rightLength)
    System.arraycopy(displayLeft, 0, mergedDisplay, 0, leftLength)
    System.arraycopy(displayRight, 0, mergedDisplay, leftLength, rightLength)
    return mergedDisplay
  }

  private final def rebalancedLeafs(displayLeft: Array[AnyRef], displayRight: Array[AnyRef], isTop: Boolean): Array[AnyRef] = {
    val leftLength = displayLeft.length
    val rightLength = displayRight.length
    if (leftLength == 32) {
      val top = new Array[AnyRef](3)
      top(0) = displayLeft
      top(1) = displayRight
      return top
    } else if (leftLength + rightLength <= 32) {
      val mergedDisplay = new Array[AnyRef](leftLength + rightLength)
      System.arraycopy(displayLeft, 0, mergedDisplay, 0, leftLength)
      System.arraycopy(displayRight, 0, mergedDisplay, leftLength, rightLength)
      val top = new Array[AnyRef](2)
      top(0) = mergedDisplay
      return top
    } else {
      val top = new Array[AnyRef](3)
      val arr0 = new Array[AnyRef](32)
      val arr1 = new Array[AnyRef](leftLength + rightLength - 32)
      top(0) = arr0
      top(1) = arr1
      System.arraycopy(displayLeft, 0, arr0, 0, leftLength)
      System.arraycopy(displayRight, 0, arr0, leftLength, 32 - leftLength)
      System.arraycopy(displayRight, 32 - leftLength, arr1, 0, rightLength - 32 + leftLength)
      return top
    }
  }

  private final def rebalancedLeafs(displayLeft: Array[Int], displayRight: Array[Int], isTop: Boolean): Array[AnyRef] = {
    val leftLength = displayLeft.length
    val rightLength = displayRight.length
    if (leftLength == 32) {
      val top = new Array[AnyRef](3)
      top(0) = displayLeft
      top(1) = displayRight
      return top
    } else if (leftLength + rightLength <= 32) {
      val mergedDisplay = new Array[Int](leftLength + rightLength)
      System.arraycopy(displayLeft, 0, mergedDisplay, 0, leftLength)
      System.arraycopy(displayRight, 0, mergedDisplay, leftLength, rightLength)
      val top = new Array[AnyRef](2)
      top(0) = mergedDisplay
      return top
    } else {
      val top = new Array[AnyRef](3)
      val arr0 = new Array[Int](32)
      val arr1 = new Array[Int](leftLength + rightLength - 32)
      top(0) = arr0
      top(1) = arr1
      System.arraycopy(displayLeft, 0, arr0, 0, leftLength)
      System.arraycopy(displayRight, 0, arr0, leftLength, 32 - leftLength)
      System.arraycopy(displayRight, 32 - leftLength, arr1, 0, rightLength - 32 + leftLength)
      return top
    }
  }

  private final def computeBranching(displayLeft: Array[AnyRef], concat: Array[AnyRef], displayRight: Array[AnyRef], currentDepth: Int) = {
    val leftLength = if (displayLeft == null) 0 else displayLeft.length - 1
    val concatLength = if (concat == null) 0 else concat.length - 1
    val rightLength = if (displayRight == null) 0 else displayRight.length - 1
    var branching = 0
    if (currentDepth == 1) {
      branching = leftLength + concatLength + rightLength
      if (leftLength != 0)
        branching -= 1
      if (rightLength != 0)
        branching -= 1
    } else {
      var i = 0
      while (i < leftLength - 1) {
        branching += displayLeft(i).asInstanceOf[Array[AnyRef]].length
        i += 1
      }
      i = 0
      while (i < concatLength) {
        branching += concat(i).asInstanceOf[Array[AnyRef]].length
        i += 1
      }
      i = 1
      while (i < rightLength) {
        branching += displayRight(i).asInstanceOf[Array[AnyRef]].length
        i += 1
      }
      if (currentDepth != 2) {
        branching -= leftLength + concatLength + rightLength
        if (leftLength != 0)
          branching += 1
        if (rightLength != 0)
          branching += 1
      }

    }
    branching
  }

  private final def takeFront0(n: Int): IntRRBVector = {
    // TODO: drop from transient leaf if n and focus are in the last leaf.
    // This would amortize takes that drop few elements like 'init()'

    if (transient) {
      normalize(depth)
      transient = false
    }

    val vec = new IntRRBVector(n)
    vec.initWithFocusFrom(this)
    if (depth > 1) {
      vec.focusOn(n - 1)
      val d0len = (vec.focus & 31) + 1
      if (d0len != 32) {
        val d0 = new Array[Int](d0len)
        System.arraycopy(vec.display0, 0, d0, 0, d0len)
        vec.display0 = d0
      }

      val cutIndex = vec.focus | vec.focusRelax
      vec.cleanTopTake(cutIndex)
      vec.focusDepth = java.lang.Math.min(vec.depth, vec.focusDepth)
      if (vec.depth > 1) {
        vec.copyDisplays(vec.focusDepth, cutIndex)
        var i = vec.depth
        var offset = 0
        while (i > vec.focusDepth) {
          val display = i match {
            case 2 => vec.display1
            case 3 => vec.display2
            case 4 => vec.display3
            case 5 => vec.display4
            case 6 => vec.display5
          }
          val oldSizes = display(display.length - 1).asInstanceOf[Array[Int]]
          val newLen = ((vec.focusRelax >> (5 * (i - 1))) & 31) + 1
          val newSizes = new Array[Int](newLen)
          System.arraycopy(oldSizes, 0, newSizes, 0, newLen - 1)
          newSizes(newLen - 1) = n - offset
          if (newLen > 1)
            offset += newSizes(newLen - 2)

          val newDisplay = new Array[AnyRef](newLen + 1)
          System.arraycopy(display, 0, newDisplay, 0, newLen)
          newDisplay(newLen - 1) = null
          newDisplay(newLen) = newSizes
          i match {
            case 2 => vec.display1 = newDisplay
            case 3 => vec.display2 = newDisplay
            case 4 => vec.display3 = newDisplay
            case 5 => vec.display4 = newDisplay
            case 6 => vec.display5 = newDisplay
          }
          i -= 1
        }
        vec.stabilizeDisplayPath(vec.depth, cutIndex)
        vec.focusEnd = n
      }
      else
        vec.focusEnd = n
      return vec
    } else if ( /* depth==1 && */ n != 32) {
      val d0 = new Array[Int](n)
      System.arraycopy(vec.display0, 0, d0, 0, n)
      vec.display0 = d0
      vec.initFocus(0, 0, n, 1, 0)
    } /* else { do nothing } */
    vec
  }

  private final def dropFront0(n: Int): IntRRBVector = {
    // TODO: drop from transient leaf if n and focus are in the first leaf
    // This would amortize drops that drop few elements like 'tail()'

    if (transient) {
      normalize(depth)
      transient = false
    }

    val vec = new IntRRBVector(this.endIndex - n)
    vec.initWithFocusFrom(this)
    if (vec.depth > 1) {
      vec.focusOn(n)
      val cutIndex = vec.focus | vec.focusRelax
      val d0Start = cutIndex & 31
      if (d0Start != 0) {
        val d0len = vec.display0.length - d0Start
        val d0 = new Array[Int](d0len)
        System.arraycopy(vec.display0, d0Start, d0, 0, d0len)
        vec.display0 = d0
      }

      vec.cleanTopDrop(cutIndex)
      if (vec.depth > 1) {
        var i = 2
        var display = vec.display1
        while (i <= vec.depth) {
          val splitStart = (cutIndex >> (5 * (i - 1))) & 31
          val newLen = display.length - splitStart - 1
          val newDisplay = new Array[AnyRef](newLen + 1)
          System.arraycopy(display, splitStart + 1, newDisplay, 1, newLen - 1)
          i match {
            case 2 =>
              newDisplay(0) = vec.display0
              vec.display1 = withComputedSizes(newDisplay, 2)
              display = vec.display2
            case 3 =>
              newDisplay(0) = vec.display1
              vec.display2 = withComputedSizes(newDisplay, 3)
              display = vec.display3
            case 4 =>
              newDisplay(0) = vec.display2
              vec.display3 = withComputedSizes(newDisplay, 4)
              display = vec.display4
            case 5 =>
              newDisplay(0) = vec.display3
              vec.display4 = withComputedSizes(newDisplay, 5)
              display = vec.display5
            case 6 =>
              newDisplay(0) = vec.display4
              vec.display5 = withComputedSizes(newDisplay, 6)
          }
          i += 1
        }
      }
      // May not be optimal, but most of the time it will be
      vec.initFocus(0, 0, vec.display0.length, 1, 0)
      return vec
    } else {
      val newLen = vec.display0.length - n
      val d0 = new Array[Int](newLen)
      System.arraycopy(vec.display0, n, d0, 0, newLen)
      vec.display0 = d0
      vec.initFocus(0, 0, newLen, 1, 0)
      return vec
    }
  }

  private[delaying] final def debugToString: String = {
    s"""
       |IntRRBVector (
       |    display0 = $display0 ${
      if (display0 != null) display0.mkString("[", ", ", "]") else ""
    }
       |    display1 = $display1 ${
      if (display1 != null) display1.mkString("[", ", ", "]") else ""
    }
       |    display2 = $display2 ${
      if (display2 != null) display2.mkString("[", ", ", "]") else ""
    }
       |    display3 = $display3 ${
      if (display3 != null) display3.mkString("[", ", ", "]") else ""
    }
       |    display4 = $display4 ${
      if (display4 != null) display4.mkString("[", ", ", "]") else ""
    }
       |    display5 = $display5 ${
      if (display5 != null) display5.mkString("[", ", ", "]") else ""
    }
       |    depth = $depth
       |    endIndex = $endIndex
       |    focus = $focus
       |    focusStart = $focusStart
       |    focusEnd = $focusEnd
       |    focusRelax = $focusRelax
       |    transient = $transient
       |)
         """.stripMargin
  }
}

final class IntRRBVectorBuilder {
  private final var display0: Array[Int] = new Array[Int](32)
  private final var display1: Array[AnyRef] = _
  private final var display2: Array[AnyRef] = _
  private final var display3: Array[AnyRef] = _
  private final var display4: Array[AnyRef] = _
  private final var display5: Array[AnyRef] = _
  private final var depth = 1
  private final var blockIndex = 0
  private final var lo = 0

  private final var acc: IntRRBVector = null

  private[delaying] final def endIndex = {
    var sz = blockIndex + lo
    if (acc != null)
      sz += acc.endIndex
    sz
  }

  final def +=(elem: Int): this.type = {
    def nextBlock() = {
      val _blockIndex = blockIndex
      val newBlockIndex = _blockIndex + 32
      blockIndex = newBlockIndex
      gotoNextBlockStartWritable(newBlockIndex ^ _blockIndex)
    }
    var _lo = lo
    if (_lo >= 32) {
      nextBlock()
      _lo = 0
    }
    display0(_lo) = elem
    lo = _lo + 1
    this
  }

  final def ++=(xs: IntRRBVector): this.type = {
    if (!xs.isEmpty) {
      if (endIndex != 0) {
        acc = this.result() ++ xs
        this.clearCurrent()
      } else if (acc != null) {
        acc = acc ++ xs
      } else {
        acc = xs
      }
    }
    this
  }

  private final def resultCurrent(): IntRRBVector = {
    val _lo = lo
    val size = blockIndex + _lo
    if (size == 0) {
      return IntRRBVector.empty
    } else {
      val resultVector = new IntRRBVector(size)
      var d0 = display0
      if (_lo != 32) {
        val d0_truncated = new Array[Int](_lo)
        System.arraycopy(d0, 0, d0_truncated, 0, _lo)
        d0 = d0_truncated
      }
      resultVector.focusEnd = size
      val _depth = depth
      resultVector.focusDepth = _depth
      val lastIndex = size - 1
      _depth match {
        case 1 =>
          resultVector.initFromDisplays(d0)
          return resultVector
        case 2 =>
          def init() = {
            val d1 = copyOfAndStabilize(display1, d0, (lastIndex >> 5) & 31)
            resultVector.initFromDisplays(d1(0).asInstanceOf[Array[Int]], d1)
          }
          init()
          return resultVector
        case 3 =>
          def init() = {
            val d1 = copyOfAndStabilize(display1, d0, (lastIndex >> 5) & 31)
            val d2 = copyOfAndStabilize(display2, d1, (lastIndex >> 10) & 31)
            val d1_0 = d2(0).asInstanceOf[Array[AnyRef]]
            val d0_0 = d1_0(0).asInstanceOf[Array[Int]]
            resultVector.initFromDisplays(d0_0, d1_0, d2)
          }
          init()
          return resultVector
        case 4 =>
          def init() = {
            val d1 = copyOfAndStabilize(display1, d0, (lastIndex >> 5) & 31)
            val d2 = copyOfAndStabilize(display2, d1, (lastIndex >> 10) & 31)
            val d3 = copyOfAndStabilize(display3, d2, (lastIndex >> 15) & 31)
            val d2_0 = d3(0).asInstanceOf[Array[AnyRef]]
            val d1_0 = d2_0(0).asInstanceOf[Array[AnyRef]]
            val d0_0 = d1_0(0).asInstanceOf[Array[Int]]
            resultVector.initFromDisplays(d0_0, d1_0, d2_0, d3)
          }
          init()
          return resultVector
        case 5 =>
          def init() = {
            val d1 = copyOfAndStabilize(display1, d0, (lastIndex >> 5) & 31)
            val d2 = copyOfAndStabilize(display2, d1, (lastIndex >> 10) & 31)
            val d3 = copyOfAndStabilize(display3, d2, (lastIndex >> 15) & 31)
            val d4 = copyOfAndStabilize(display4, d3, (lastIndex >> 20) & 31)
            val d3_0 = d4(0).asInstanceOf[Array[AnyRef]]
            val d2_0 = d3_0(0).asInstanceOf[Array[AnyRef]]
            val d1_0 = d2_0(0).asInstanceOf[Array[AnyRef]]
            val d0_0 = d1_0(0).asInstanceOf[Array[Int]]
            resultVector.initFromDisplays(d0_0, d1_0, d2_0, d3_0, d4)
          }
          init()
          return resultVector
        case 6 =>
          def init() = {
            val d1 = copyOfAndStabilize(display1, d0, (lastIndex >> 5) & 31)
            val d2 = copyOfAndStabilize(display2, d1, (lastIndex >> 10) & 31)
            val d3 = copyOfAndStabilize(display3, d2, (lastIndex >> 15) & 31)
            val d4 = copyOfAndStabilize(display4, d3, (lastIndex >> 20) & 31)
            val d5 = copyOfAndStabilize(display5, d4, (lastIndex >> 25) & 31)
            val d4_0 = d5(0).asInstanceOf[Array[AnyRef]]
            val d3_0 = d4_0(0).asInstanceOf[Array[AnyRef]]
            val d2_0 = d3_0(0).asInstanceOf[Array[AnyRef]]
            val d1_0 = d2_0(0).asInstanceOf[Array[AnyRef]]
            val d0_0 = d1_0(0).asInstanceOf[Array[Int]]
            resultVector.initFromDisplays(d0_0, d1_0, d2_0, d3_0, d4_0, d5)
          }
          init()
          return resultVector
      }
    }
  }

  private final def copyOfAndStabilize(array: Array[AnyRef], lastChild: AnyRef, indexOfLastChild: Int) = {
    val newArray = new Array[AnyRef](indexOfLastChild + 2)
    System.arraycopy(array, 0, newArray, 0, indexOfLastChild)
    newArray(indexOfLastChild) = lastChild
    newArray
  }

  final def result(): IntRRBVector = {
    val resultVector =
      if (acc == null) resultCurrent()
      else resultWithAcc()
    resultVector
  }

  private final def resultWithAcc() = {
    acc ++ resultCurrent()
  }

  private final def clearCurrent(): Unit = {
    display0 = new Array[Int](32)
    display1 = null
    display2 = null
    display3 = null
    display4 = null
    display5 = null
    depth = 1
    blockIndex = 0
    lo = 0
  }

  final def clear(): Unit = {
    clearCurrent()
    acc = null
  }

  private final def gotoNextBlockStartWritable(xor: Int): Unit = {
    if (xor < 1024) {
      def gotoNextBlockStartWritable() = {
        val d1: Array[AnyRef] =
          if (depth == 1) {
            depth = 2
            val d1 = new Array[AnyRef](33)
            d1(0) = display0
            display1 = d1
            d1
          } else {
            display1
          }
        val d0 = new Array[Int](32)
        display0 = d0
        d1((blockIndex >> 5) & 31) = d0
      }
      gotoNextBlockStartWritable()
      return
    } else if (xor < 32768) {
      def gotoNextBlockStartWritable() = {
        val d2: Array[AnyRef] =
          if (depth == 2) {
            depth = 3
            val d2 = new Array[AnyRef](33)
            d2(0) = display1
            display2 = d2
            d2
          } else display2
        val d0 = new Array[Int](32)
        val d1 = new Array[AnyRef](33)
        display0 = d0
        display1 = d1
        val index = blockIndex
        d1((index >> 5) & 31) = d0
        d2((index >> 10) & 31) = d1
      }
      gotoNextBlockStartWritable()
      return
    } else if (xor < 1048576) {
      def gotoNextBlockStartWritable() = {
        val d3: Array[AnyRef] =
          if (depth == 3) {
            depth = 4
            val d3 = new Array[AnyRef](33)
            d3(0) = display2
            display3 = d3
            d3
          } else display3
        val d0 = new Array[Int](32)
        val d1 = new Array[AnyRef](33)
        val d2 = new Array[AnyRef](33)
        display0 = d0
        display1 = d1
        display2 = d2
        val index = blockIndex
        d1((index >> 5) & 31) = d0
        d2((index >> 10) & 31) = d1
        d3((index >> 15) & 31) = d2
      }
      gotoNextBlockStartWritable()
      return
    } else if (xor < 33554432) {
      def gotoNextBlockStartWritable() = {
        val d4: Array[AnyRef] =
          if (depth == 4) {
            depth = 5
            val d4 = new Array[AnyRef](33)
            d4(0) = display3
            display4 = d4
            d4
          } else display4
        val d0 = new Array[Int](32)
        val d1 = new Array[AnyRef](33)
        val d2 = new Array[AnyRef](33)
        val d3 = new Array[AnyRef](33)
        display0 = d0
        display1 = d1
        display2 = d2
        display3 = d3
        val index = blockIndex
        d1((index >> 5) & 31) = d0
        d2((index >> 10) & 31) = d1
        d3((index >> 15) & 31) = d2
        d4((index >> 20) & 31) = d3
      }
      gotoNextBlockStartWritable()
      return
    } else if (xor < 1073741824) {
      def gotoNextBlockStartWritable() = {
        val d5: Array[AnyRef] =
          if (depth == 5) {
            depth = 6
            val d5 = new Array[AnyRef](33)
            d5(0) = display4
            display5 = d5
            d5
          } else display5
        val d0 = new Array[Int](32)
        val d1 = new Array[AnyRef](33)
        val d2 = new Array[AnyRef](33)
        val d3 = new Array[AnyRef](33)
        val d4 = new Array[AnyRef](33)
        display0 = d0
        display1 = d1
        display2 = d2
        display3 = d3
        display4 = d4
        val index = blockIndex
        d1((index >> 5) & 31) = d0
        d2((index >> 10) & 31) = d1
        d3((index >> 15) & 31) = d2
        d4((index >> 20) & 31) = d3
        d5((index >> 25) & 31) = d4
      }
      gotoNextBlockStartWritable()
      return
    } else
      throw new IllegalArgumentException()
  }
}

class IntRRBVectorIterator(startIndex: Int, override private[delaying] val endIndex: Int) extends IntRRBVectorPointer {
  /* Index in the vector of the first element of current block, i.e. current display0 */
  private final var blockIndex: Int = _
  /* Index in current block, i.e. current display0 */
  private final var lo: Int = _
  /* End index (or length) of current block, i.e. current display0 */
  private final var endLo: Int = _
  private final var _hasNext: Boolean = _

  private[delaying] final def initIteratorFrom(that: IntRRBVectorPointer): Unit = {
    initWithFocusFrom(that)
    _hasNext = startIndex < endIndex
    if (_hasNext) {
      focusOn(startIndex)
      blockIndex = focusStart + (focus & -32)
      lo = focus & 31
      if (endIndex < focusEnd)
        focusEnd = endIndex
      endLo = java.lang.Math.min(focusEnd - blockIndex, 32)
      return
    } else {
      blockIndex = 0
      lo = 0
      endLo = 1
      display0 = new Array[Int](1)
    }
  }

  final def hasNext = _hasNext

  final def next(): Int = {
    // keep method size under 35 bytes, so that it can be JIT-inlined
    var _lo = lo
    val res: Int = display0(_lo)
    _lo += 1
    lo = _lo
    if (_lo == endLo)
      gotoNextBlock()
    res
  }

  private[delaying] final def gotoNextBlock(): Unit = {
    val oldBlockIndex = blockIndex
    val newBlockIndex = oldBlockIndex + endLo
    blockIndex = newBlockIndex
    lo = 0
    val _focusEnd = focusEnd
    if (newBlockIndex < _focusEnd) {
      val _focusStart = focusStart
      val newBlockIndexInFocus = newBlockIndex - _focusStart
      gotoNextBlockStart(newBlockIndexInFocus, newBlockIndexInFocus ^ (oldBlockIndex - _focusStart))
      endLo = java.lang.Math.min(_focusEnd - newBlockIndex, 32)
      return
    } else {
      val _endIndex = endIndex
      if (newBlockIndex < _endIndex) {
        focusOn(newBlockIndex)
        if (_endIndex < focusEnd)
          focusEnd = _endIndex
        endLo = java.lang.Math.min(focusEnd - newBlockIndex, 32)
        return
      } else {
        /* setup dummy index that will not fail with IndexOutOfBound in subsequent 'next()' invocations */
        lo = 0
        blockIndex = _endIndex
        endLo = 1
        if (_hasNext) {
          _hasNext = false
          return
        }
        else throw new NoSuchElementException("reached iterator end")
      }
    }
  }

  def foreach(f: Int => Unit): Unit = {
    while (hasNext)
      f(next())
  }

  private[delaying] def remaining: Int = java.lang.Math.max(endIndex - (blockIndex + lo), 0)

}

class IntRRBVectorReverseIterator(startIndex: Int, final override private[delaying] val endIndex: Int) extends IntRRBVectorPointer {
  private final var lastIndexOfBlock: Int = _
  private final var lo: Int = _
  private final var endLo: Int = _
  private final var _hasNext: Boolean = _

  private[delaying] final def initIteratorFrom(that: IntRRBVectorPointer): Unit = {
    initWithFocusFrom(that)
    _hasNext = startIndex < endIndex
    if (_hasNext) {
      val idx = endIndex - 1
      focusOn(idx)
      lastIndexOfBlock = idx
      lo = (idx - focusStart) & 31
      endLo = java.lang.Math.max(startIndex - focusStart - lastIndexOfBlock, 0)
      return
    } else {
      lastIndexOfBlock = 0
      lo = 0
      endLo = 0
      display0 = new Array[Int](1)
    }
  }

  final def hasNext = _hasNext

  final def next(): Int = {
    // TODO push the check of _hasNext and the throwing of the NoSuchElementException into gotoPrevBlock() like in the normal IntRRBVectorIterator
    if (_hasNext) {
      var _lo = lo
      val res = display0(_lo).asInstanceOf[Int]
      _lo -= 1
      lo = _lo
      if (_lo < endLo)
        gotoPrevBlock()
      res
    } else
      throw new NoSuchElementException("reached iterator end")
  }

  private[delaying] final def gotoPrevBlock(): Unit = {
    val newBlockIndex = lastIndexOfBlock - 32
    if (focusStart <= newBlockIndex) {
      val _focusStart = focusStart
      val newBlockIndexInFocus = newBlockIndex - _focusStart
      gotoPrevBlockStart(newBlockIndexInFocus, newBlockIndexInFocus ^ (lastIndexOfBlock - _focusStart))
      lastIndexOfBlock = newBlockIndex
      lo = 31
      endLo = java.lang.Math.max(startIndex - focusStart - focus, 0)
      return
    } else if (startIndex < focusStart) {
      val newIndex = focusStart - 1
      focusOn(newIndex)
      lastIndexOfBlock = newIndex
      lo = (newIndex - focusStart) & 31
      endLo = java.lang.Math.max(startIndex - focusStart - lastIndexOfBlock, 0)
      return
    } else {
      _hasNext = false
    }
  }
}

private[delaying] trait IntRRBVectorPointer {
  private[delaying] final var display0: Array[Int] = _
  private[delaying] final var display1: Array[AnyRef] = _
  private[delaying] final var display2: Array[AnyRef] = _
  private[delaying] final var display3: Array[AnyRef] = _
  private[delaying] final var display4: Array[AnyRef] = _
  private[delaying] final var display5: Array[AnyRef] = _
  private[delaying] final var depth: Int = _
  private[delaying] final var focusStart: Int = 0
  private[delaying] final var focusEnd: Int = 0
  private[delaying] final var focusDepth: Int = 0
  private[delaying] final var focus: Int = 0
  private[delaying] final var focusRelax: Int = 0

  private[delaying] def endIndex: Int

  private[delaying] final def initWithFocusFrom(that: IntRRBVectorPointer): Unit = {
    initFocus(that.focus, that.focusStart, that.focusEnd, that.focusDepth, that.focusRelax)
    initFrom(that)
  }

  private[delaying] final def initFocus[U](focus: Int, focusStart: Int, focusEnd: Int, focusDepth: Int, focusRelax: Int): Unit = {
    this.focus = focus
    this.focusStart = focusStart
    this.focusEnd = focusEnd
    this.focusDepth = focusDepth
    this.focusRelax = focusRelax
  }

  private[delaying] final def initFromLeaf(leaf: Array[Int]): Unit = {
    display0 = leaf
    this.depth = 1
    focusEnd = focusStart
    focusOn(0)
  }

  private[delaying] final def initFromRoot(root: Array[AnyRef], depth: Int): Unit = {
    depth match {
      case 2 => display1 = root
      case 3 => display2 = root
      case 4 => display3 = root
      case 5 => display4 = root
      case 6 => display5 = root
    }
    this.depth = depth
    focusEnd = focusStart
    focusOn(0)
  }

  private[delaying] final def initFromDisplays(display0: Array[Int]): Unit = {
    this.depth = 1
    this.display0 = display0
  }

  private[delaying] final def initFromDisplays(display0: Array[Int], display1: Array[AnyRef]): Unit = {
    this.depth = 2
    this.display0 = display0
    this.display1 = display1
  }

  private[delaying] final def initFromDisplays(display0: Array[Int], display1: Array[AnyRef], display2: Array[AnyRef]): Unit = {
    this.depth = 3
    this.display0 = display0
    this.display1 = display1
    this.display2 = display2
  }

  private[delaying] final def initFromDisplays(display0: Array[Int], display1: Array[AnyRef], display2: Array[AnyRef], display3: Array[AnyRef]): Unit = {
    this.depth = 4
    this.display0 = display0
    this.display1 = display1
    this.display2 = display2
    this.display3 = display3
  }

  private[delaying] final def initFromDisplays(display0: Array[Int], display1: Array[AnyRef], display2: Array[AnyRef], display3: Array[AnyRef], display4: Array[AnyRef]): Unit = {
    this.depth = 5
    this.display0 = display0
    this.display1 = display1
    this.display2 = display2
    this.display3 = display3
    this.display4 = display4
  }

  private[delaying] final def initFromDisplays(display0: Array[Int], display1: Array[AnyRef], display2: Array[AnyRef], display3: Array[AnyRef], display4: Array[AnyRef], display5: Array[AnyRef]): Unit = {
    this.depth = 6
    this.display0 = display0
    this.display1 = display1
    this.display1 = display2
    this.display3 = display3
    this.display4 = display4
    this.display5 = display5
  }


  private[delaying] final def initFrom(that: IntRRBVectorPointer): Unit = {
    depth = that.depth
    that.depth match {
      case 0 =>
        return
      case 1 =>
        this.display0 = that.display0
        return
      case 2 =>
        this.display0 = that.display0
        this.display1 = that.display1
        return
      case 3 =>
        this.display0 = that.display0
        this.display1 = that.display1
        this.display2 = that.display2
        return
      case 4 =>
        this.display0 = that.display0
        this.display1 = that.display1
        this.display2 = that.display2
        this.display3 = that.display3
        return
      case 5 =>
        this.display0 = that.display0
        this.display1 = that.display1
        this.display2 = that.display2
        this.display3 = that.display3
        this.display4 = that.display4
        return
      case 6 =>
        this.display0 = that.display0
        this.display1 = that.display1
        this.display2 = that.display2
        this.display3 = that.display3
        this.display4 = that.display4
        this.display5 = that.display5
        return
      case _ => throw new IllegalStateException()
    }
  }

  private[delaying] final def initSingleton(elem: Int): Unit = {
    initFocus(0, 0, 1, 1, 0)
    val d0 = new Array[Int](1)
    d0(0) = elem
    display0 = d0
    depth = 1
  }

  private[delaying] final def root(): AnyRef = depth match {
    case 0 => return null
    case 1 => return display0
    case 2 => return display1
    case 3 => return display2
    case 4 => return display3
    case 5 => return display4
    case 6 => return display5
    case _ => throw new IllegalStateException()
  }

  private[delaying] final def focusOn(index: Int): Unit = {
    val _focusStart = focusStart
    if (_focusStart <= index && index < focusEnd) {
      val indexInFocus = index - _focusStart
      val xor = indexInFocus ^ focus
      if (xor >= 32)
        gotoPos(indexInFocus, xor)
      focus = indexInFocus
      return
    } else {
      gotoPosFromRoot(index)
    }
  }

  private[delaying] final def getElementFromRoot(index: Int): Int = {
    var indexInSubTree = index
    var currentDepth = depth
    var display: Array[AnyRef] = currentDepth match {
      case 2 => display1
      case 3 => display2
      case 4 => display3
      case 5 => display4
      case 6 => display5
    }

    var sizes = display(display.length - 1).asInstanceOf[Array[Int]]
    do {
      val sizesIdx = getIndexInSizes(sizes, indexInSubTree)
      if (sizesIdx != 0)
        indexInSubTree -= sizes(sizesIdx - 1)
      display = display(sizesIdx).asInstanceOf[Array[AnyRef]]
      if (currentDepth > 3)
        sizes = display(display.length - 1).asInstanceOf[Array[Int]]
      else
        sizes = null
      currentDepth -= 1
    } while (sizes != null)



    currentDepth match {
      case 1 =>
        val sizesIdx = getIndexInSizes(sizes, indexInSubTree)
        if (sizesIdx != 0)
          indexInSubTree -= sizes(sizesIdx - 1)
        val d0 = display(sizesIdx).asInstanceOf[Array[Int]]
        return getElem0(d0, indexInSubTree)
      case 2 => return getElem1(display, indexInSubTree)
      case 3 => return getElem2(display, indexInSubTree)
      case 4 => return getElem3(display, indexInSubTree)
      case 5 => return getElem4(display, indexInSubTree)
      case 6 => return getElem5(display, indexInSubTree)
      case _ => throw new IllegalStateException
    }
  }

  @inline private final def getIndexInSizes(sizes: Array[Int], indexInSubTree: Int): Int = {
    if (indexInSubTree == 0)
      return 0
    var is = 0
    while (sizes(is) <= indexInSubTree)
      is += 1
    is
  }

  private[delaying] final def gotoPosFromRoot(index: Int): Unit = {
    var _startIndex: Int = 0
    var _endIndex: Int = endIndex
    var currentDepth: Int = depth
    var _focusRelax: Int = 0
    var continue: Boolean = currentDepth > 1

    if (continue) {
      var display: Array[AnyRef] = currentDepth match {
        case 2 => display1
        case 3 => display2
        case 4 => display3
        case 5 => display4
        case 6 => display5
        case _ => throw new IllegalStateException()
      }
      do {
        val sizes = display(display.length - 1).asInstanceOf[Array[Int]]
        if (sizes == null) {
          continue = false
        } else {
          val is = getIndexInSizes(sizes, index - _startIndex)
          if (currentDepth == 2) {
            display0 = display(is).asInstanceOf[Array[Int]]
            continue = false
          } else {
            display = display(is).asInstanceOf[Array[AnyRef]]
            currentDepth match {
              case 3 => display1 = display
              case 4 => display2 = display
              case 5 => display3 = display
              case 6 => display4 = display
            }
          }
          if (is < sizes.length - 1)
            _endIndex = _startIndex + sizes(is)

          if (is != 0)
            _startIndex += sizes(is - 1)

          currentDepth -= 1
          _focusRelax |= is << (5 * currentDepth)
        }
      } while (continue)
    }
    val indexInFocus = index - _startIndex
    gotoPos(indexInFocus, 1 << (5 * (currentDepth - 1)))
    initFocus(indexInFocus, _startIndex, _endIndex, currentDepth, _focusRelax)
  }

  private[delaying] final def makeTransientSizes(oldSizes: Array[Int], transientBranchIndex: Int): Array[Int] = {
    val newSizes = new Array[Int](oldSizes.length)
    var delta = oldSizes(transientBranchIndex)
    if (transientBranchIndex > 0) {
      delta -= oldSizes(transientBranchIndex - 1)
      if (!oldSizes.eq(newSizes))
        System.arraycopy(oldSizes, 0, newSizes, 0, transientBranchIndex)
    }
    var i = transientBranchIndex
    val len = newSizes.length
    while (i < len) {
      newSizes(i) = oldSizes(i) - delta
      i += 1
    }
    newSizes
  }

  private final def makeNewRoot0(node: Array[AnyRef]): Array[AnyRef] = {
    val newRoot = new Array[AnyRef](3)
    newRoot(0) = node
    val dLen = node.length
    val dSizes = node(dLen - 1)
    if (dSizes != null) {
      val newRootSizes = new Array[Int](2)
      val dSize = dSizes.asInstanceOf[Array[Int]](dLen - 2)
      newRootSizes(0) = dSize
      newRootSizes(1) = dSize
      newRoot(2) = newRootSizes
    }
    newRoot
  }

  private final def makeNewRoot1(node: Array[AnyRef], currentDepth: Int): Array[AnyRef] = {
    val dSize = treeSize(node, currentDepth - 1)
    val newRootSizes = new Array[Int](2)
    /* newRootSizes(0) = 0 */
    newRootSizes(1) = dSize
    val newRoot = new Array[AnyRef](3)
    newRoot(1) = node
    newRoot(2) = newRootSizes
    newRoot
  }

  private final def copyAndIncRightRoot(node: Array[AnyRef], transient: Boolean, currentLevel: Int): Array[AnyRef] = {
    val len = node.length
    val newRoot = copyOf(node, len - 1, len + 1)
    val oldSizes = node(len - 1).asInstanceOf[Array[Int]]
    if (oldSizes != null) {
      val newSizes = new Array[Int](len)

      System.arraycopy(oldSizes, 0, newSizes, 0, len - 1)

      if (transient) {
        newSizes(len - 1) = 1 << (5 * currentLevel)
      }
      newSizes(len - 1) = newSizes(len - 2)
      newRoot(len) = newSizes
    }
    newRoot
  }

  private final def copyAndIncLeftRoot(node: Array[AnyRef], transient: Boolean, currentLevel: Int): Array[AnyRef] = {
    val len = node.length
    val newRoot = new Array[AnyRef](len + 1)
    System.arraycopy(node, 0, newRoot, 1, len - 1)

    val oldSizes = node(len - 1)
    val newSizes = new Array[Int](len)
    if (oldSizes != null) {
      if (transient) {
        System.arraycopy(oldSizes, 1, newSizes, 2, len - 2)
      } else {
        System.arraycopy(oldSizes, 0, newSizes, 1, len - 1)
      }
    } else {
      val subTreeSize = 1 << (5 * currentLevel)
      var acc = 0
      var i = 1
      while (i < len - 1) {
        acc += subTreeSize
        newSizes(i) = acc
        i += 1
      }
      newSizes(i) = acc + treeSize(node(node.length - 2).asInstanceOf[Array[AnyRef]], currentLevel)
    }
    newRoot(len) = newSizes
    newRoot
  }


  private[delaying] final def setupNewBlockInNextBranch(xor: Int, transient: Boolean): Unit = {
    if (xor < 1024) {
      if (depth == 1) {
        depth = 2
        val newRoot = new Array[AnyRef](3)
        newRoot(0) = display0
        display1 = newRoot
      } else {
        val newRoot = copyAndIncRightRoot(display1, transient, 1)
        if (transient) {
          val oldTransientBranch = newRoot.length - 3
          newRoot(oldTransientBranch) = display0
          withRecomputeSizes(newRoot, 2, oldTransientBranch)
        }
        display1 = newRoot
      }
      display0 = new Array(1)
      return
    } else if (xor < 32768) {
      if (transient)
        normalize(2)
      if (depth == 2) {
        depth = 3
        display2 = makeNewRoot0(display1)
      } else {
        val newRoot = copyAndIncRightRoot(display2, transient, 2)
        if (transient) {
          val oldTransientBranch = newRoot.length - 3
          newRoot(oldTransientBranch) = display1
          withRecomputeSizes(newRoot, 3, oldTransientBranch)
        }
        display2 = newRoot
      }
      display0 = new Array(1)
      display1 = IntRRBVector.emptyTransientBlock
      return
    } else if (xor < 1048576) {
      if (transient)
        normalize(3)
      if (depth == 3) {
        depth = 4
        display3 = makeNewRoot0(display2)
      } else {
        val newRoot = copyAndIncRightRoot(display3, transient, 3)
        if (transient) {
          val transientBranch = newRoot.length - 3
          newRoot(transientBranch) = display2
          withRecomputeSizes(newRoot, 4, transientBranch)
        }
        display3 = newRoot
      }
      display0 = new Array(1)
      val _emptyTransientBlock = IntRRBVector.emptyTransientBlock
      display1 = _emptyTransientBlock // new Array(2)
      display2 = _emptyTransientBlock // new Array(2)
      return
    } else if (xor < 33554432) {
      if (transient)
        normalize(4)
      if (depth == 4) {
        depth = 5
        display4 = makeNewRoot0(display3)
      } else {
        val newRoot = copyAndIncRightRoot(display4, transient, 4)
        if (transient) {
          val transientBranch = newRoot.length - 3
          newRoot(transientBranch) = display3
          withRecomputeSizes(newRoot, 5, transientBranch)
        }
        display4 = newRoot
      }

      display0 = new Array(1)
      val _emptyTransientBlock = IntRRBVector.emptyTransientBlock
      display1 = _emptyTransientBlock // new Array(2)
      display2 = _emptyTransientBlock // new Array(2)
      display3 = _emptyTransientBlock // new Array(2)
      return
    } else if (xor < 1073741824) {
      if (transient)
        normalize(5)
      if (depth == 5) {
        depth = 6
        display5 = makeNewRoot0(display4)
      } else {
        val newRoot = copyAndIncRightRoot(display5, transient, 5)
        if (transient) {
          val transientBranch = newRoot.length - 3
          newRoot(transientBranch) = display4
          withRecomputeSizes(newRoot, 6, transientBranch)
        }
        display5 = newRoot
      }
      display0 = new Array(1)
      val _emptyTransientBlock = IntRRBVector.emptyTransientBlock
      display1 = _emptyTransientBlock // new Array(2)
      display2 = _emptyTransientBlock // new Array(2)
      display3 = _emptyTransientBlock // new Array(2)
      display4 = _emptyTransientBlock // new Array(2)
      return
    } else
      throw new IllegalArgumentException()
  }

  private[delaying] final def setupNewBlockInInitBranch(insertionDepth: Int, transient: Boolean): Unit = {
    insertionDepth match {
      case 2 =>
        if (depth == 1) {
          depth = 2
          val sizes = new Array[Int](2)
          /* sizes(0) = 0 */
          sizes(1) = display0.length
          val newRoot = new Array[AnyRef](3)
          newRoot(1) = display0
          newRoot(2) = sizes
          display1 = newRoot
        } else {
          val newRoot = copyAndIncLeftRoot(display1, transient, 1)
          if (transient) {
            withRecomputeSizes(newRoot, 2, 1)
            newRoot(1) = display0
          }
          display1 = newRoot
        }
        display0 = new Array[Int](1)
        return
      case 3 =>
        if (transient)
          normalize(2)
        if (depth == 2) {
          depth = 3
          display2 = makeNewRoot1(display1, 3)
        } else {
          val newRoot = copyAndIncLeftRoot(display2, transient, 2)
          if (transient) {
            withRecomputeSizes(newRoot, 3, 1)
            newRoot(1) = display1
          }
          display2 = newRoot
        }
        val _emptyTransientBlock = IntRRBVector.emptyTransientBlock
        display1 = _emptyTransientBlock // new Array[AnyRef](2)
        display0 = new Array[Int](1)
        return
      case 4 =>
        if (transient)
          normalize(3)
        if (depth == 3) {
          depth = 4
          display3 = makeNewRoot1(display2, 4)
        } else {
          val newRoot = copyAndIncLeftRoot(display3, transient, 3)
          if (transient) {
            withRecomputeSizes(newRoot, 4, 1)
            newRoot(1) = display2
          }
          display3 = newRoot
        }
        val _emptyTransientBlock = IntRRBVector.emptyTransientBlock
        display2 = _emptyTransientBlock // new Array[AnyRef](2)
        display1 = _emptyTransientBlock // new Array[AnyRef](2)
        display0 = new Array[Int](1)
        return
      case 5 =>
        if (transient)
          normalize(4)
        if (depth == 4) {
          depth = 5
          display4 = makeNewRoot1(display3, 5)
        } else {
          val newRoot = copyAndIncLeftRoot(display4, transient, 4)
          if (transient) {
            withRecomputeSizes(newRoot, 5, 1)
            newRoot(1) = display3
          }
          display4 = newRoot
        }
        val _emptyTransientBlock = IntRRBVector.emptyTransientBlock
        display3 = _emptyTransientBlock // new Array[AnyRef](2)
        display2 = _emptyTransientBlock // new Array[AnyRef](2)
        display1 = _emptyTransientBlock // new Array[AnyRef](2)
        display0 = new Array[Int](1)
        return
      case 6 =>
        if (transient)
          normalize(5)
        if (depth == 5) {
          depth = 6
          display5 = makeNewRoot1(display4, 6)
        } else {
          val newRoot = copyAndIncLeftRoot(display5, transient, 5)
          if (transient) {
            withRecomputeSizes(newRoot, 6, 1)
            newRoot(1) = display4
          }
          display5 = newRoot
        }
        val _emptyTransientBlock = IntRRBVector.emptyTransientBlock
        display4 = _emptyTransientBlock // new Array[AnyRef](2)
        display3 = _emptyTransientBlock // new Array[AnyRef](2)
        display2 = _emptyTransientBlock // new Array[AnyRef](2)
        display1 = _emptyTransientBlock // new Array[AnyRef](2)
        display0 = new Array[Int](1)
        return
      case _ => throw new IllegalStateException()
    }
  }

  private[delaying] final def getElem(index: Int, xor: Int): Int = {
    if (xor < 32) return getElem0(display0, index)
    else if (xor < 1024) return getElem1(display1, index)
    else if (xor < 32768) return getElem2(display2, index)
    else if (xor < 1048576) return getElem3(display3, index)
    else if (xor < 33554432) return getElem4(display4, index)
    else if (xor < 1073741824) return getElem5(display5, index)
    else throw new IllegalArgumentException(xor.toString)
  }

  private final def getElem0(display: Array[Int], index: Int): Int =
    display(index & 31)

  private final def getElem1(display: Array[AnyRef], index: Int): Int =
    display((index >> 5) & 31).asInstanceOf[Array[Int]](index & 31)

  private final def getElem2(display: Array[AnyRef], index: Int): Int =
    display((index >> 10) & 31).asInstanceOf[Array[AnyRef]]((index >> 5) & 31).asInstanceOf[Array[Int]](index & 31)

  private final def getElem3(display: Array[AnyRef], index: Int): Int =
    display((index >> 15) & 31).asInstanceOf[Array[AnyRef]]((index >> 10) & 31).asInstanceOf[Array[AnyRef]]((index >> 5) & 31).asInstanceOf[Array[Int]](index & 31)

  private final def getElem4(display: Array[AnyRef], index: Int): Int =
    display((index >> 20) & 31).asInstanceOf[Array[AnyRef]]((index >> 15) & 31).asInstanceOf[Array[AnyRef]]((index >> 10) & 31).asInstanceOf[Array[AnyRef]]((index >> 5) & 31).asInstanceOf[Array[Int]](index & 31)

  private final def getElem5(display: Array[AnyRef], index: Int): Int =
    display((index >> 25) & 31).asInstanceOf[Array[AnyRef]]((index >> 20) & 31).asInstanceOf[Array[AnyRef]]((index >> 15) & 31).asInstanceOf[Array[AnyRef]]((index >> 10) & 31).asInstanceOf[Array[AnyRef]]((index >> 5) & 31).asInstanceOf[Array[Int]](index & 31)

  private[delaying] final def gotoPos(index: Int, xor: Int): Unit = {
    if (xor < 32)
      return
    else if (xor < 1024) {
      display0 = display1((index >> 5) & 31).asInstanceOf[Array[Int]]
      return
    } else if (xor < 32768) {
      val d1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      display1 = d1
      display0 = d1((index >> 5) & 31).asInstanceOf[Array[Int]]
      return
    } else if (xor < 1048576) {
      val d2 = display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      display2 = d2
      val d1 = d2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      display1 = d1
      display0 = d1((index >> 5) & 31).asInstanceOf[Array[Int]]
      return
    } else if (xor < 33554432) {
      val d3 = display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      display3 = d3
      val d2 = d3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      display2 = d2
      val d1 = d2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      display1 = d1
      display0 = d1((index >> 5) & 31).asInstanceOf[Array[Int]]
      return
    } else if (xor < 1073741824) {
      val d4 = display5((index >> 25) & 31).asInstanceOf[Array[AnyRef]]
      display4 = d4
      val d3 = d4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      display3 = d3
      val d2 = d3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      display2 = d2
      val d1 = d2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      display1 = d1
      display0 = d1((index >> 5) & 31).asInstanceOf[Array[Int]]
      return
    } else
      throw new IllegalArgumentException()
  }

  private[delaying] final def gotoNextBlockStart(index: Int, xor: Int): Unit = {
    if (xor < 1024) {
      display0 = display1((index >> 5) & 31).asInstanceOf[Array[Int]]
      return
    } else if (xor < 32768) {
      val d1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      display1 = d1
      display0 = d1(0).asInstanceOf[Array[Int]]
      return
    } else if (xor < 1048576) {
      val d2 = display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      val d1 = d2(0).asInstanceOf[Array[AnyRef]]
      display0 = d1(0).asInstanceOf[Array[Int]]
      display1 = d1
      display2 = d2
      return
    } else if (xor < 33554432) {
      val d3 = display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      val d2 = d3(0).asInstanceOf[Array[AnyRef]]
      val d1 = d2(0).asInstanceOf[Array[AnyRef]]
      display0 = d1(0).asInstanceOf[Array[Int]]
      display1 = d1
      display2 = d2
      display3 = d3
      return
    } else if (xor < 1073741824) {
      val d4 = display5((index >> 25) & 31).asInstanceOf[Array[AnyRef]]
      val d3 = d4(0).asInstanceOf[Array[AnyRef]]
      val d2 = d3(0).asInstanceOf[Array[AnyRef]]
      val d1 = d2(0).asInstanceOf[Array[AnyRef]]
      display4 = d4
      display3 = d3
      display2 = d2
      display1 = d1
      display0 = d1(0).asInstanceOf[Array[Int]]
      return
    } else
      throw new IllegalArgumentException()
  }

  private[delaying] final def gotoPrevBlockStart(index: Int, xor: Int): Unit = {
    if (xor < 1024) {
      display0 = display1((index >> 5) & 31).asInstanceOf[Array[Int]]
      return
    } else if (xor < 32768) {
      val d1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
      display1 = d1
      display0 = d1(31).asInstanceOf[Array[Int]]
      return
    } else if (xor < 1048576) {
      val d2 = display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
      display2 = d2
      val d1 = d2(31).asInstanceOf[Array[AnyRef]]
      display1 = d1
      display0 = d1(31).asInstanceOf[Array[Int]]
      return
    } else if (xor < 33554432) {
      val d3 = display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
      display3 = d3
      val d2 = d3(31).asInstanceOf[Array[AnyRef]]
      display2 = d2
      val d1 = d2(31).asInstanceOf[Array[AnyRef]]
      display1 = d1
      display0 = d1(31).asInstanceOf[Array[Int]]
      return
    } else if (xor < 1073741824) {
      val d4 = display5((index >> 25) & 31).asInstanceOf[Array[AnyRef]]
      display4 = d4
      val d3 = d4(31).asInstanceOf[Array[AnyRef]]
      display3 = d3
      val d2 = d3(31).asInstanceOf[Array[AnyRef]]
      display2 = d2
      val d1 = d2(31).asInstanceOf[Array[AnyRef]]
      display1 = d1
      display0 = d1(31).asInstanceOf[Array[Int]]
      return
    } else
      throw new IllegalArgumentException()
  }

  private[delaying] final def normalize(_depth: Int): Unit = {
    val _focusDepth = focusDepth
    val stabilizationIndex = focus | focusRelax
    copyDisplaysAndStabilizeDisplayPath(_focusDepth, stabilizationIndex)

    var currentLevel = _focusDepth
    if (currentLevel < _depth) {
      var display = currentLevel match {
        case 1 => display1
        case 2 => display2
        case 3 => display3
        case 4 => display4
        case 5 => display5
      }
      do {
        val newDisplay = copyOf(display)
        val idx = (stabilizationIndex >> (5 * currentLevel)) & 31
        currentLevel match {
          case 1 =>
            newDisplay(idx) = display0
            display1 = withRecomputeSizes(newDisplay, 2, idx)
            display = display2
          case 2 =>
            newDisplay(idx) = display1
            display2 = withRecomputeSizes(newDisplay, 3, idx)
            display = display3
          case 3 =>
            newDisplay(idx) = display2
            display3 = withRecomputeSizes(newDisplay, 4, idx)
            display = display4
          case 4 =>
            newDisplay(idx) = display3
            display4 = withRecomputeSizes(newDisplay, 5, idx)
            display = display5
          case 5 =>
            newDisplay(idx) = display4
            display5 = withRecomputeSizes(newDisplay, 6, idx)
        }
        currentLevel += 1
      } while (currentLevel < _depth)
    }
  }

  private[delaying] final def copyDisplays(_depth: Int, _focus: Int): Unit = {
    if (_depth < 2) return
    else {
      val idx1 = ((_focus >> 5) & 31) + 1
      display1 = copyOf(display1, idx1, idx1 + 1)
      if (_depth < 3) return
      else {
        val idx2 = ((_focus >> 10) & 31) + 1
        display2 = copyOf(display2, idx2, idx2 + 1)
        if (_depth < 4) return
        else {
          val idx3 = ((_focus >> 15) & 31) + 1
          display3 = copyOf(display3, idx3, idx3 + 1)
          if (_depth < 5) return
          else {
            val idx4 = ((_focus >> 20) & 31) + 1
            display4 = copyOf(display4, idx4, idx4 + 1)
            if (_depth < 6) return
            else {
              val idx5 = ((_focus >> 25) & 31) + 1
              display5 = copyOf(display5, idx5, idx5 + 1)
            }
          }
        }
      }
    }
  }

  private[delaying] final def copyDisplaysAndNullFocusedBranch(_depth: Int, _focus: Int): Unit = {
    _depth match {
      case 2 =>
        display1 = copyOfAndNull(display1, (_focus >> 5) & 31)
        return
      case 3 =>
        display1 = copyOfAndNull(display1, (_focus >> 5) & 31)
        display2 = copyOfAndNull(display2, (_focus >> 10) & 31)
        return
      case 4 =>
        display1 = copyOfAndNull(display1, (_focus >> 5) & 31)
        display2 = copyOfAndNull(display2, (_focus >> 10) & 31)
        display3 = copyOfAndNull(display3, (_focus >> 15) & 31)
        return
      case 5 =>
        display1 = copyOfAndNull(display1, (_focus >> 5) & 31)
        display2 = copyOfAndNull(display2, (_focus >> 10) & 31)
        display3 = copyOfAndNull(display3, (_focus >> 15) & 31)
        display4 = copyOfAndNull(display4, (_focus >> 20) & 31)
        return
      case 6 =>
        display1 = copyOfAndNull(display1, (_focus >> 5) & 31)
        display2 = copyOfAndNull(display2, (_focus >> 10) & 31)
        display3 = copyOfAndNull(display3, (_focus >> 15) & 31)
        display4 = copyOfAndNull(display4, (_focus >> 20) & 31)
        display5 = copyOfAndNull(display5, (_focus >> 25) & 31)
        return
    }
  }

  private final def copyDisplaysAndStabilizeDisplayPath(_depth: Int, _focus: Int): Unit = {
    _depth match {
      case 1 =>
        return
      case 2 =>
        val d1 = copyOf(display1)
        d1((_focus >> 5) & 31) = display0
        display1 = d1
        return
      case 3 =>
        val d1 = copyOf(display1)
        d1((_focus >> 5) & 31) = display0
        display1 = d1
        val d2 = copyOf(display2)
        d2((_focus >> 10) & 31) = d1
        display2 = d2
        return
      case 4 =>
        val d1 = copyOf(display1)
        d1((_focus >> 5) & 31) = display0
        display1 = d1
        val d2 = copyOf(display2)
        d2((_focus >> 10) & 31) = d1
        display2 = d2
        val d3 = copyOf(display3)
        d3((_focus >> 15) & 31) = d2
        display3 = d3
        return
      case 5 =>
        val d1 = copyOf(display1)
        d1((_focus >> 5) & 31) = display0
        display1 = d1
        val d2 = copyOf(display2)
        d2((_focus >> 10) & 31) = d1
        display2 = d2
        val d3 = copyOf(display3)
        d3((_focus >> 15) & 31) = d2
        display3 = d3
        val d4 = copyOf(display4)
        d4((_focus >> 20) & 31) = d3
        display4 = d4
        return
      case 6 =>
        val d1 = copyOf(display1)
        d1((_focus >> 5) & 31) = display0
        display1 = d1
        val d2 = copyOf(display2)
        d2((_focus >> 10) & 31) = d1
        display2 = d2
        val d3 = copyOf(display3)
        d3((_focus >> 15) & 31) = d2
        display3 = d3
        val d4 = copyOf(display4)
        d4((_focus >> 20) & 31) = d3
        display4 = d4
        val d5 = copyOf(display5)
        d5((_focus >> 25) & 31) = d4
        display5 = d5
        return
    }
  }

  private[delaying] final def copyDisplaysTop(currentDepth: Int, _focusRelax: Int): Unit = {
    var _currentDepth = currentDepth
    while (_currentDepth < this.depth) {
      _currentDepth match {
        case 2 =>
          val cutIndex = (_focusRelax >> 5) & 31
          display1 = copyOf(display1, cutIndex + 1, cutIndex + 2)
        case 3 =>
          val cutIndex = (_focusRelax >> 10) & 31
          display2 = copyOf(display2, cutIndex + 1, cutIndex + 2)
        case 4 =>
          val cutIndex = (_focusRelax >> 15) & 31
          display3 = copyOf(display3, cutIndex + 1, cutIndex + 2)
        case 5 =>
          val cutIndex = (_focusRelax >> 20) & 31
          display4 = copyOf(display4, cutIndex + 1, cutIndex + 2)
        case 6 =>
          val cutIndex = (_focusRelax >> 25) & 31
          display5 = copyOf(display5, cutIndex + 1, cutIndex + 2)
        case _ => throw new IllegalStateException()
      }
      _currentDepth += 1
    }
  }

  private[delaying] final def stabilizeDisplayPath(_depth: Int, _focus: Int): Unit = {
    if (_depth <= 1)
      return
    else {
      val d1 = display1
      d1((_focus >> 5) & 31) = display0
      if (_depth <= 2)
        return
      else {
        val d2 = display2
        d2((_focus >> 10) & 31) = d1
        if (_depth <= 3)
          return
        else {
          val d3 = display3
          d3((_focus >> 15) & 31) = d2
          if (_depth <= 4)
            return
          else {
            val d4 = display4
            d4((_focus >> 20) & 31) = d3
            if (_depth > 5) {
              display5((_focus >> 25) & 31) = d4
            }
          }
        }
      }
    }
  }

  private[delaying] final def cleanTopTake(cutIndex: Int): Unit = this.depth match {
    case 2 =>
      var newDepth = 0
      if (cutIndex < 32) {
        display1 = null
        newDepth = 1
      } else
        newDepth = 2
      this.depth = newDepth
      return
    case 3 =>
      var newDepth = 0
      if (cutIndex < 1024) {
        display2 = null
        if (cutIndex < 32) {
          display1 = null
          newDepth = 1
        } else
          newDepth = 2
      } else
        newDepth = 3
      this.depth = newDepth
      return
    case 4 =>
      var newDepth = 0
      if (cutIndex < 32768) {
        display3 = null
        if (cutIndex < 1024) {
          display2 = null
          if (cutIndex < 32) {
            display1 = null
            newDepth = 1
          } else
            newDepth = 2
        } else
          newDepth = 3
      } else
        newDepth = 4
      this.depth = newDepth
      return
    case 5 =>
      var newDepth = 0
      if (cutIndex < 1048576) {
        display4 = null
        if (cutIndex < 32768) {
          display3 = null
          if (cutIndex < 1024) {
            display2 = null
            if (cutIndex < 32) {
              display1 = null
              newDepth = 1
            } else
              newDepth = 2
          } else
            newDepth = 3
        } else
          newDepth = 4
      } else
        newDepth = 5
      this.depth = newDepth
      return
    case 6 =>
      var newDepth = 0
      if (cutIndex < 33554432) {
        display5 = null
        if (cutIndex < 1048576) {
          display4 = null
          if (cutIndex < 32768) {
            display3 = null
            if (cutIndex < 1024) {
              display2 = null
              if (cutIndex < 32) {
                display1 = null
                newDepth = 1
              } else
                newDepth = 2
            } else
              newDepth = 3
          } else
            newDepth = 4
        } else
          newDepth = 5
      } else
        newDepth = 6
      this.depth = newDepth
      return
  }

  private[delaying] final def cleanTopDrop(cutIndex: Int): Unit = this.depth match {
    case 2 =>
      var newDepth = 0
      if ((cutIndex >> 5) == display1.length - 2) {
        display1 = null
        newDepth = 1
      } else
        newDepth = 2
      this.depth = newDepth
      return
    case 3 =>
      var newDepth = 0
      if ((cutIndex >> 10) == display2.length - 2) {
        display2 = null
        if ((cutIndex >> 5) == display1.length - 2) {
          display1 = null
          newDepth = 1
        } else
          newDepth = 2
      } else
        newDepth = 3
      this.depth = newDepth
      return
    case 4 =>
      var newDepth = 0
      if ((cutIndex >> 15) == display3.length - 2) {
        display3 = null
        if ((cutIndex >> 10) == display2.length - 2) {
          display2 = null
          if ((cutIndex >> 5) == display1.length - 2) {
            display1 = null
            newDepth = 1
          } else
            newDepth = 2
        } else
          newDepth = 3
      } else
        newDepth = 4
      this.depth = newDepth
      return
    case 5 =>
      var newDepth = 0
      if ((cutIndex >> 20) == display4.length - 2) {
        display4 = null
        if ((cutIndex >> 15) == display3.length - 2) {
          display3 = null
          if ((cutIndex >> 10) == display2.length - 2) {
            display2 = null
            if ((cutIndex >> 5) == display1.length - 2) {
              display1 = null
              newDepth = 1
            } else
              newDepth = 2
          } else
            newDepth = 3
        } else
          newDepth = 4
      } else
        newDepth = 5
      this.depth = newDepth
      return
    case 6 =>
      var newDepth = 0
      if ((cutIndex >> 25) == display5.length - 2) {
        display5 = null
        if ((cutIndex >> 20) == display4.length - 2) {
          display4 = null
          if ((cutIndex >> 15) == display3.length - 2) {
            display3 = null
            if ((cutIndex >> 10) == display2.length - 2) {
              display2 = null
              if ((cutIndex >> 5) == display1.length - 2) {
                display1 = null
                newDepth = 1
              } else
                newDepth = 2
            } else
              newDepth = 3
          } else
            newDepth = 4
        } else
          newDepth = 5
      } else
        newDepth = 6
      this.depth = newDepth
      return
  }

  private[delaying] final def copyOf(array: Array[AnyRef], numElements: Int, newSize: Int) = {
    val newArray = new Array[AnyRef](newSize)
    System.arraycopy(array, 0, newArray, 0, numElements)
    newArray
  }

  private[delaying] final def copyOf(array: Array[Int], numElements: Int, newSize: Int) = {
    val newArray = new Array[Int](newSize)
    System.arraycopy(array, 0, newArray, 0, numElements)
    newArray
  }

  private[delaying] final def copyOfAndNull(array: Array[AnyRef], nullIndex: Int) = {
    val len = array.length
    val newArray = new Array[AnyRef](len)
    System.arraycopy(array, 0, newArray, 0, len - 1)
    newArray(nullIndex) = null
    val sizes = array(len - 1).asInstanceOf[Array[Int]]
    if (sizes != null) {
      newArray(len - 1) = makeTransientSizes(sizes, nullIndex)
    }
    newArray
  }

  private[delaying] final def copyOf(array: Array[AnyRef]): Array[AnyRef] = {
    val len = array.length
    val newArray = new Array[AnyRef](len)
    System.arraycopy(array, 0, newArray, 0, len)
    newArray
  }

  private[delaying] final def copyOf(array: Array[Int]): Array[Int] = {
    val len = array.length
    val newArray = new Array[Int](len)
    System.arraycopy(array, 0, newArray, 0, len)
    newArray
  }

  protected final def withRecomputeSizes(node: Array[AnyRef], currentDepth: Int, branchToUpdate: Int): Array[AnyRef] = {
    val end = node.length - 1
    val oldSizes = node(end).asInstanceOf[Array[Int]]
    if (oldSizes != null) {
      val newSizes = new Array[Int](end)

      val delta = treeSize(node(branchToUpdate).asInstanceOf[Array[AnyRef]], currentDepth - 1)
      if (branchToUpdate > 0)
        System.arraycopy(oldSizes, 0, newSizes, 0, branchToUpdate)
      var i = branchToUpdate
      while (i < end) {
        newSizes(i) = oldSizes(i) + delta
        i += 1
      }
      if (notBalanced(node, newSizes, currentDepth, end))
        node(end) = newSizes
    }
    node
  }

  protected final def withComputedSizes(node: Array[AnyRef], currentDepth: Int): Array[AnyRef] = {
    var i = 0
    var acc = 0
    val end = node.length - 1
    if (end > 1) {
      val sizes = new Array[Int](end)
      while (i < end) {
        acc += treeSize(node(i).asInstanceOf[Array[AnyRef]], currentDepth - 1)
        sizes(i) = acc
        i += 1
      }
      if (notBalanced(node, sizes, currentDepth, end))
        node(end) = sizes
    } else if (end == 1 && currentDepth > 2) {
      val child = node(0).asInstanceOf[Array[AnyRef]]
      val childSizes = child(child.length - 1).asInstanceOf[Array[Int]]
      if (childSizes != null) {
        if (childSizes.length != 1) {
          val sizes = new Array[Int](1)
          sizes(0) = childSizes(childSizes.length - 1)
          node(end) = sizes
        } else {
          node(end) = childSizes
        }
      }
    }
    node
  }

  protected final def withComputedSizes1(node: Array[AnyRef]): Array[AnyRef] = {
    var i = 0
    var acc = 0
    val end = node.length - 1
    if (end > 1) {
      val sizes = new Array[Int](end)
      while (i < end) {
        acc += node(i).asInstanceOf[Array[AnyRef]].length
        sizes(i) = acc
        i += 1
      }
      if /* node is not balanced */ (sizes(end - 2) != ((end - 1) << 5))
        node(end) = sizes
    }
    node
  }

  @inline private final def notBalanced(node: Array[AnyRef], sizes: Array[Int], currentDepth: Int, end: Int): Boolean = {
    (end == 1 || sizes(end - 2) != ((end - 1) << (5 * (currentDepth - 1)))) || (
      (currentDepth > 2) && {
        val last = node(end - 1).asInstanceOf[Array[AnyRef]]
        last(last.length - 1) != null
      }
      )
  }

  private final def treeSize(node: Array[AnyRef], currentDepth: Int): Int = {
    def treeSizeRec(node: Array[AnyRef], currentDepth: Int, acc: Int): Int = {
      if (currentDepth == 1) {
        return acc + node.length
      } else {
        val treeSizes = node(node.length - 1).asInstanceOf[Array[Int]]
        if (treeSizes != null)
          return acc + treeSizes(treeSizes.length - 1)
        else {
          val len = node.length
          return treeSizeRec(node(len - 2).asInstanceOf[Array[AnyRef]], currentDepth - 1, acc + (len - 2) * (1 << (5 * (currentDepth - 1))))
        }
      }
    }
    treeSizeRec(node, currentDepth, 0)
  }
}
