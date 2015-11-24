package me.d_d.delaying

import scala.annotation.tailrec

class DArray(var left: Array[Array[Int]], var right: Array[Array[Int]]){
  // println(s"creating DArray[\n  left = ${left.map(x => if (x eq null) "null" else x.mkString("(",", ",")")).mkString("(",", ",")")}, \n right = ${right.map(x => if (x eq null) "null" else x.mkString("(",", ",")")).mkString("(",", ",")")}]")
                                         // 0         1  2         3   4  5  6     7 8        x
                                         // 32        31 30        30 29  29 29    29 30      numzero(x)
                                         // 31        30 30        29 29  29 29     30 30      numzero(x+1)

                                         // 0         0  0         1  0  1   2     3 0
                                         // 0         0  1         0  1 2 3        0 1
  assert(left.zipWithIndex.forall(x => (x._1 eq null) || (x._1.length == math.pow(2, x._2))))
  var leftSize = arraySize(left)

                                    //       7  8 9 10    11 12   13
                                    //       6  5 4 3     2  1    0                           y =  leftSize + rightSize - x -1
                                    //       29 29 29 30   30 31    32                                numzero(y)
                                    //       29 29 29 29   30 30    31                                numzero(leftSize + rightSize - x)
  assert(right.zipWithIndex.forall(x => (x._1 eq null) || (x._1.length == math.pow(2, x._2))))

  var rightSize = arraySize(right)

  private def arraySize(arg: Array[Array[Int]]): Int = {
    var i = 0
    var acc = 0
    while (i < arg.length) {
      if(arg(i) ne null)
        acc = acc + arg(i).length
      i = i + 1
    }
    acc
  }

  final def larraySize(outerIdx: Int) = {
    31 - java.lang.Integer.numberOfLeadingZeros(outerIdx + 1)
  }

  def getById(id: Int, sumLength: Int, arrays: Array[Array[Int]]) = {
    val subst1 = if (id > 0) sumLength & (java.lang.Integer.highestOneBit(id) - 1) else 0
    var r = id - subst1          //  this trick gave 20% speedup
    var c = sumLength - subst1   //  todo: could we improve it?
    val subst2 = r & c
    r = r - subst2
    c = c - subst2
    var b = Integer.lowestOneBit(c)
    while (r >= b && r != 0) {
      r = r - b
      c = c - b
      b = Integer.lowestOneBit(c)
    }
    arrays(Integer.numberOfTrailingZeros(c))(r)
  }

  final def apply(idx: Int) = {
    if (idx < leftSize) getById(idx, leftSize, left)
    else getById(leftSize + rightSize - idx - 1, rightSize, right)
  }

  final def rebalance() =
    if (leftSize > (rightSize + 1) * 2 || rightSize > (leftSize + 1) * 2) {

      // determine new sizes
      val newRightSize = rightSize / 2 + leftSize / 2

      val newLeftSize = leftSize + rightSize - newRightSize
      val leftArraySize = larraySize(newLeftSize - 1) + 1
      val rightArraySize = larraySize(newRightSize - 1) + 1

      def rellocateNewArray(elemCount: Int, arraySize: Int): Array[Array[Int]] = {
        val arr = new Array[Array[Int]](arraySize)
        var i = elemCount
        var j = 0
        var sz = 1
        while(i > 0) {
          if ((i & 1) == 1)
            arr(j) = new Array[Int](sz)
          i = i / 2
          sz = sz * 2
          j = j + 1
        }
        arr
      }


      val newLeft = rellocateNewArray(newLeftSize, leftArraySize)
      var newRight = rellocateNewArray(newRightSize, rightArraySize)


      var rollingSourceArrayId = 0
      var rollingSourceElemId = 0

      var rollingTargetArrayId = 0
      var rollingTargetElemId = 0

      // copy from left to newLeft.

      while(rollingSourceArrayId < left.length && rollingTargetArrayId < newLeft.length) {
        if((left(rollingSourceArrayId) eq null) || (rollingSourceElemId == left(rollingSourceArrayId).length)) {
          rollingSourceArrayId = rollingSourceArrayId + 1
          rollingSourceElemId = 0
        }
        else if((newLeft(rollingTargetArrayId) eq null) || (rollingTargetElemId == newLeft(rollingTargetArrayId).length)) {
          rollingTargetArrayId = rollingTargetArrayId + 1
          rollingTargetElemId = 0
        }
        else {
          val toCopy = java.lang.Math.min(
            left(rollingSourceArrayId).length - rollingSourceElemId,
            newLeft(rollingTargetArrayId).length - rollingTargetElemId)
          System.arraycopy(left(rollingSourceArrayId), rollingSourceElemId, newLeft(rollingTargetArrayId), rollingTargetElemId, toCopy)
          rollingSourceElemId = rollingSourceElemId + toCopy
          rollingTargetElemId = rollingTargetElemId + toCopy
        }
      }

      // copy from left to newRight
      if (rollingSourceArrayId < left.length) {
        rollingTargetArrayId = newRight.length - 1
        if(newRight(rollingTargetArrayId) ne null)
          rollingTargetElemId = newRight(rollingTargetArrayId).length - 1

        while(rollingSourceArrayId < left.length) {    // todo: move out length
          if((left(rollingSourceArrayId) eq null) || (rollingSourceElemId == left(rollingSourceArrayId).length)) {
            rollingSourceArrayId = rollingSourceArrayId + 1
            rollingSourceElemId = 0
          }
          else if((newRight(rollingTargetArrayId) eq null) || (rollingTargetElemId == -1)) {
            rollingTargetArrayId = rollingTargetArrayId - 1
            if ((rollingTargetArrayId != -1) && (right(rollingTargetArrayId) ne null))
              rollingTargetElemId = newRight(rollingTargetArrayId).length - 1
          }
          else {
            val toCopy = java.lang.Math.min(
              left(rollingSourceArrayId).length - rollingSourceElemId,
              rollingTargetElemId + 1)
            var i = 0
            val source = left(rollingSourceArrayId)
            val target = newRight(rollingTargetArrayId)
            while(i < toCopy) {
              target(rollingTargetElemId - i) = source(rollingSourceElemId + i)
              i = i + 1
            }

            rollingSourceElemId = rollingSourceElemId + toCopy
            rollingTargetElemId = rollingTargetElemId - toCopy
          }
        }
        rollingSourceArrayId = right.length - 1
        if ((rollingSourceArrayId != -1) && (right(rollingSourceArrayId) ne null))
          rollingSourceElemId = right(rollingSourceArrayId).length - 1

      } else if(rollingTargetArrayId < newLeft.length) {
        // copy from right to newLeft
        rollingSourceArrayId = right.length - 1
        if (right(rollingSourceArrayId) ne null) rollingSourceElemId = right(rollingSourceArrayId).length - 1

        while (rollingTargetArrayId < newLeft.length) {
          if ((right(rollingSourceArrayId) eq null) || (rollingSourceElemId == -1)) {
            rollingSourceArrayId = rollingSourceArrayId - 1
            if ((rollingSourceArrayId != -1) && (right(rollingSourceArrayId) ne null))
              rollingSourceElemId = right(rollingSourceArrayId).length - 1
          }
          else if ((newLeft(rollingTargetArrayId) eq null) || (rollingTargetElemId == newLeft(rollingTargetArrayId).length)) {
            rollingTargetArrayId = rollingTargetArrayId + 1
            rollingTargetElemId = 0
          }
          else {
            val toCopy = java.lang.Math.min(
              newLeft(rollingTargetArrayId).length - rollingTargetElemId,
              rollingSourceElemId + 1)
            var i = 0
            val source = right(rollingSourceArrayId)
            val target = newLeft(rollingTargetArrayId)
            while (i < toCopy) {
              target(rollingTargetElemId + i) = source(rollingSourceElemId - i)
              i = i + 1
            }

            rollingSourceElemId = rollingSourceElemId - toCopy
            rollingTargetElemId = rollingTargetElemId + toCopy
          }

        }

        rollingTargetArrayId = newRight.length - 1
        if(newRight(rollingTargetArrayId) ne null) rollingTargetElemId = newRight(rollingTargetArrayId).length - 1
      }

      // copy from right to newRight

      while(rollingSourceArrayId != -1 && rollingTargetArrayId != -1) {
        if ((right(rollingSourceArrayId) eq null) || (rollingSourceElemId == -1)) {
          rollingSourceArrayId = rollingSourceArrayId - 1
          if ((rollingSourceArrayId != -1) && (right(rollingSourceArrayId) ne null)) rollingSourceElemId = right(rollingSourceArrayId).length - 1
        }
        else if((newRight(rollingTargetArrayId) eq null) || (rollingTargetElemId == -1)) {
          rollingTargetArrayId = rollingTargetArrayId - 1
          if((rollingTargetArrayId != -1) && (newRight(rollingTargetArrayId) ne null)) rollingTargetElemId = newRight(rollingTargetArrayId).length - 1
        }
        else {
          val toCopy = java.lang.Math.min(
            rollingSourceElemId + 1,
            rollingTargetElemId + 1)
          System.arraycopy(right(rollingSourceArrayId), rollingSourceElemId - toCopy + 1, newRight(rollingTargetArrayId), rollingTargetElemId - toCopy + 1, toCopy)
          rollingSourceElemId = rollingSourceElemId - toCopy
          rollingTargetElemId = rollingTargetElemId - toCopy
        }
      }

      // finally publish changes

      leftSize = newLeftSize
      rightSize = newRightSize
      left = newLeft
      right = newRight
    }


  def append(el: Int): DArray = {
    val newLeftSize = this.leftSize
    val newRightSize = this.rightSize + 1
    val newRightArraySize = larraySize(this.rightSize) + 1
    val rightArray = new Array[Array[Int]](newRightArraySize)

    var newArrayIdx = 0
    var sz = 1
    while((newArrayIdx < right.length) && (right(newArrayIdx) ne null)) {
      newArrayIdx = newArrayIdx + 1
      sz = sz * 2
    }

    var rollingSourceArrayId = 0

    var rollingTargetElemId = 1
    val target = new Array[Int](sz)
    target(0) = el // this is the actual append :-)
    while(rollingSourceArrayId < newArrayIdx) {
      System.arraycopy(right(rollingSourceArrayId),0,target, rollingTargetElemId, right(rollingSourceArrayId).length)
      rollingTargetElemId = rollingTargetElemId + right(rollingSourceArrayId).length
      rollingSourceArrayId = rollingSourceArrayId + 1
    }
    rightArray(newArrayIdx) = target


    if(newArrayIdx < right.length)
      System.arraycopy(right, newArrayIdx + 1, rightArray, newArrayIdx + 1, right.length - newArrayIdx - 1)

    val rez = new DArray(left, rightArray)
    rez.rebalance
    rez
  }

  def prepend(el: Int): DArray = {
    val newLeftSize = this.leftSize + 1
    val newRightSize = this.rightSize
    val newLeftArraySize = larraySize(this.leftSize) + 1
    val leftArray = new Array[Array[Int]](newLeftArraySize)

    var newArrayIdx = 0
    var sz = 1
    while((newArrayIdx < left.length) && (left(newArrayIdx) ne null)) {
      newArrayIdx = newArrayIdx + 1
      sz = sz * 2
    }

    var rollingSourceArrayId = 0

    var rollingTargetElemId = 1
    val target = new Array[Int](sz)
    target(0) = el // this is the actual append :-)
    while(rollingSourceArrayId < newArrayIdx) {
      System.arraycopy(left(rollingSourceArrayId),0, target, rollingTargetElemId, left(rollingSourceArrayId).length)
      rollingTargetElemId = rollingTargetElemId + left(rollingSourceArrayId).length
      rollingSourceArrayId = rollingSourceArrayId + 1
    }
    leftArray(newArrayIdx) = target


    if(newArrayIdx < left.length)
      System.arraycopy(left, newArrayIdx + 1, leftArray, newArrayIdx + 1, left.length - newArrayIdx - 1)

    val rez = new DArray(leftArray, right)
    rez.rebalance()
    rez
  }

  def foreach(f : scala.Function1[Int, Unit]): scala.Unit = {

    var rollingSourceArrayId = 0

    while(rollingSourceArrayId < left.length) {
      if(left(rollingSourceArrayId) ne null) {
        var i = 0
        while (i < left(rollingSourceArrayId).length) {
          f.apply(left(rollingSourceArrayId)(i))
          i = i + 1
        }

      }
      rollingSourceArrayId = rollingSourceArrayId + 1
    }

    rollingSourceArrayId = right.length - 1

    while(rollingSourceArrayId >= 0) {
      if(right(rollingSourceArrayId) ne null) {
        var i = right(rollingSourceArrayId).length - 1
        while (i >= 0) {
          f.apply(right(rollingSourceArrayId)(i))
          i = i - 1
        }

      }
      rollingSourceArrayId = rollingSourceArrayId - 1
    }
  }

  def iterator: Iterator[Int] = {
    val t = new Iterator[Int] {
      private var rollingSourceArrayId = 0
      private var elemIdx = -1

      @tailrec
      def advance: Unit = {
        elemIdx = elemIdx + 1
        if (rollingSourceArrayId >= 0) {
          if (rollingSourceArrayId == left.length) {
            rollingSourceArrayId = -right.length
          } else if ((left(rollingSourceArrayId) eq null) || (elemIdx >= left(rollingSourceArrayId).length)) {
            rollingSourceArrayId = rollingSourceArrayId + 1
            elemIdx = -1
            advance
          } else {} // done
        } else {
          if (rollingSourceArrayId == -1) {
            if (elemIdx > 0) () // finish
          } else if ((right(-rollingSourceArrayId - 1) eq null) || elemIdx >= right(-rollingSourceArrayId - 1).length) {
            rollingSourceArrayId = rollingSourceArrayId + 1
            elemIdx = -1
            advance
          } else {} // done
        }
      }

      def hasNext: Boolean = {
        if (elemIdx == -1) advance
        !(rollingSourceArrayId == -1 && ((right(0) eq null) || elemIdx > 0))
      }


      def next(): Int = {
        if (!hasNext) ???
        else {
          var r =
            if (rollingSourceArrayId >= 0)
              left(rollingSourceArrayId)(elemIdx)
            else right(-rollingSourceArrayId - 1)(right(-rollingSourceArrayId - 1).length - 1  - elemIdx)
          advance
          r
        }
      }
    }
    t
  }

}


object DArray{
  val empty = new DArray(Array.empty, Array.empty)
  def apply(elems: Int*) = { //todo: rewrite
    var t = empty
    for(elem <- elems) {
      t = t.append(elem)
    }
    t
  }

  def applyr(elems: Int*) = { //todo: rewrite
  var t = empty
    for(elem <- elems.reverse) {
      t = t.prepend(elem)
    }
    t
  }
}
