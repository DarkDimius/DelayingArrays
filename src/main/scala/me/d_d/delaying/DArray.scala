package me.d_d.delaying

import java.util

import me.d_d.delaying.DArray.Update
import me.d_d.delaying.DArray.Update

import scala.annotation.{switch, tailrec}

class DArray(var left: Array[Array[Int]], var right: Array[Array[Int]], val headsNTails: Array[Int], val headsSize: Int, val tailsSize: Int, val updates: util.IdentityHashMap[Array[Int], Update] = null, val history: Update = null){
  import DArray._

  //println(s"creating DArray[\n  left = ${left.map(x => if (x eq null) "null" else x.mkString("(",", ",")")).mkString("(",", ",")")}, \n right = ${right.map(x => if (x eq null) "null" else x.mkString("(",", ",")")).mkString("(",", ",")")}, headsNTails: ${headsNTails.mkString("(",", ",")")}]")
                                         // 0         1  2         3   4  5  6     7 8        x
                                         // 32        31 30        30 29  29 29    29 30      numzero(x)
                                         // 31        30 30        29 29  29 29     30 30      numzero(x+1)

                                         // 0         0  0         1  0  1   2     3 0
                                         // 0         0  1         0  1 2 3        0 1
  assert(left.zipWithIndex.forall(x => (x._1 eq null) || (x._1.length == ELEMS_FIRST_ARRAY * math.pow(2, x._2))))
  private var leftSize = arraySize(left)

                                    //       7  8 9 10    11 12   13
                                    //       6  5 4 3     2  1    0                           y =  leftSize + rightSize - x -1
                                    //       29 29 29 30   30 31    32                                numzero(y)
                                    //       29 29 29 29   30 30    31                                numzero(leftSize + rightSize - x)
  assert(right.zipWithIndex.forall(x => (x._1 eq null) || (x._1.length == ELEMS_FIRST_ARRAY * math.pow(2, x._2))))

  private var rightSize = arraySize(right)

  /*private var l0: Array[Int] = _
  private var l1: Array[Int] = _
  private var l2: Array[Int] = _
  private var l3: Array[Int] = _
  private var l4: Array[Int] = _
  private var l5: Array[Int] = _
  private var l6: Array[Int] = _
  private var l7: Array[Int] = _
  private var l8: Array[Int] = _
  private var l9: Array[Int] = _
  private var l10: Array[Int] = _
  private var l11: Array[Int] = _
  private var l12: Array[Int] = _
  private var l13: Array[Int] = _
  private var l14: Array[Int] = _
  private var l15: Array[Int] = _
  private var l16: Array[Int] = _
  private var l17: Array[Int] = _
  private var l18: Array[Int] = _
  private var l19: Array[Int] = _
  private var l20: Array[Int] = _
  private var l21: Array[Int] = _
  private var l22: Array[Int] = _
  private var l23: Array[Int] = _
  private var l24: Array[Int] = _
  private var l25: Array[Int] = _
  private var l26: Array[Int] = _
  private var l27: Array[Int] = _
  private var l28: Array[Int] = _
  private var l29: Array[Int] = _
  private var l30: Array[Int] = _
  private var l31: Array[Int] = _

  private var r0: Array[Int] = _
  private var r1: Array[Int] = _
  private var r2: Array[Int] = _
  private var r3: Array[Int] = _
  private var r4: Array[Int] = _
  private var r5: Array[Int] = _
  private var r6: Array[Int] = _
  private var r7: Array[Int] = _
  private var r8: Array[Int] = _
  private var r9: Array[Int] = _
  private var r10: Array[Int] = _
  private var r11: Array[Int] = _
  private var r12: Array[Int] = _
  private var r13: Array[Int] = _
  private var r14: Array[Int] = _
  private var r15: Array[Int] = _
  private var r16: Array[Int] = _
  private var r17: Array[Int] = _
  private var r18: Array[Int] = _
  private var r19: Array[Int] = _
  private var r20: Array[Int] = _
  private var r21: Array[Int] = _
  private var r22: Array[Int] = _
  private var r23: Array[Int] = _
  private var r24: Array[Int] = _
  private var r25: Array[Int] = _
  private var r26: Array[Int] = _
  private var r27: Array[Int] = _
  private var r28: Array[Int] = _
  private var r29: Array[Int] = _
  private var r30: Array[Int] = _
  private var r31: Array[Int] = _


  assignFields()

  private def assignFields(): Unit = {
    if (left.length > 0)  l0 = left(0)
    if (left.length > 1) l1 = left(1)
    if (left.length > 2)  l2 = left(2)
    if (left.length > 3) l3 = left(3)
    if (left.length > 4) l4 = left(4)
    if (left.length > 5) l5 = left(5)
    if (left.length > 6) l6 = left(6)
    if (left.length > 7) l7 = left(7)
    if (left.length > 8) l8 = left(8)
    if (left.length > 9) l9 = left(9)
    if (left.length > 10) l10 = left(10)
    if (left.length > 11) l11 = left(11)
    if (left.length > 12) l12 = left(12)
    if (left.length > 13) l13 = left(13)
    if (left.length > 14) l14 = left(14)
    if (left.length > 15) l15 = left(15)
    if (left.length > 16) l16 = left(16)
    if (left.length > 17) l17 = left(17)
    if (left.length > 18) l18 = left(18)
    if (left.length > 19) l19 = left(19)
    if (left.length > 20) l20 = left(20)
    if (left.length > 21) l21 = left(21)
    if (left.length > 22) l22 = left(22)
    if (left.length > 23) l23 = left(23)
    if (left.length > 24) l24 = left(24)
    if (left.length > 25) l25 = left(25)
    if (left.length > 26) l26 = left(26)
    if (left.length > 27) l27 = left(27)
    if (left.length > 28) l28 = left(28)
    if (left.length > 29) l29 = left(29)
    if (left.length > 30) l30 = left(30)
    if (left.length > 31) l31 = left(31)

    if (right.length > 0) r0 = right(0)
    if (right.length > 1) r1 = right(1)
    if (right.length > 2) r2 = right(2)
    if (right.length > 3) r3 = right(3)
    if (right.length > 4) r4 = right(4)
    if (right.length > 5) r5 = right(5)
    if (right.length > 6) r6 = right(6)
    if (right.length > 7) r7 = right(7)
    if (right.length > 8) r8 = right(8)
    if (right.length > 9) r9 = right(9)
    if (right.length > 10) r10 = right(10)
    if (right.length > 11) r11 = right(11)
    if (right.length > 12) r12 = right(12)
    if (right.length > 13) r13 = right(13)
    if (right.length > 14) r14 = right(14)
    if (right.length > 15) r15 = right(15)
    if (right.length > 16) r16 = right(16)
    if (right.length > 17) r17 = right(17)
    if (right.length > 18) r18 = right(18)
    if (right.length > 19) r19 = right(19)
    if (right.length > 20) r20 = right(20)
    if (right.length > 21) r21 = right(21)
    if (right.length > 22) r22 = right(22)
    if (right.length > 23) r23 = right(23)
    if (right.length > 24) r24 = right(24)
    if (right.length > 25) r25 = right(25)
    if (right.length > 26) r26 = right(26)
    if (right.length > 27) r27 = right(27)
    if (right.length > 28) r28 = right(28)
    if (right.length > 29) r29 = right(29)
    if (right.length > 30) r30 = right(30)
    if (right.length > 31) r31 = right(31)

  }

  @inline final def getLeft(idx: Int) = (idx: @switch) match {
    case 0 => l0
    case 1 => l1
    case 2 => l2
    case 3 => l3
    case 4 => l4
    case 5 => l5
    case 6 => l6
    case 7 => l7
    case 8 => l8
    case 9 => l9
    case 10 => l10
    case 11 => l11
    case 12 => l12
    case 13 => l13
    case 14 => l14
    case 15 => l15
    case 16 => l16
    case 17 => l17
    case 18 => l18
    case 19 => l19
    case 20 => l20
    case 21 => l21
    case 22 => l22
    case 23 => l23
    case 24 => l24
    case 25 => l25
    case 26 => l26
    case 27 => l27
    case 28 => l28
    case 29 => l29
    case 30 => l30
    case 31 => l31
  }

  @inline final def getRight(idx: Int) = (idx: @switch) match {
    case 0 => r0
    case 1 => r1
    case 2 => r2
    case 3 => r3
    case 4 => r4
    case 5 => r5
    case 6 => r6
    case 7 => r7
    case 8 => r8
    case 9 => r9
    case 10 => r10
    case 11 => r11
    case 12 => r12
    case 13 => r13
    case 14 => r14
    case 15 => r15
    case 16 => r16
    case 17 => r17
    case 18 => r18
    case 19 => r19
    case 20 => r20
    case 21 => r21
    case 22 => r22
    case 23 => r23
    case 24 => r24
    case 25 => r25
    case 26 => r26
    case 27 => r27
    case 28 => r28
    case 29 => r29
    case 30 => r30
    case 31 => r31
  }

   // sumLength: 8 + 4 + 2 + 0
  // id = 2 + 1

  def getByIdL(idx: Int, sumLength: Int) = {
    val id = idx + 1
    val lId = Integer.numberOfLeadingZeros(id)
    val mask = -1 >>> lId
    val maskedSum = sumLength & mask

    var arrayId = 0
    var elemId = 0

    if (maskedSum < id) {
      arrayId = Integer.numberOfTrailingZeros(sumLength & ~mask)
      elemId = idx - maskedSum
      //println(s"1 id = $id, sumLength = $sumLength -> ($arrayId $elemId)")
    } else {
      arrayId = 31 - lId
      elemId = idx - (sumLength & (mask >>> 1))
      // println(s"2 id = $id, sumLength = $sumLength -> ($arrayId $elemId)")
    }

    var arr = (arrayId: @switch) match {
      case 0 => l0
      case 1 => l1
      case 2 => l2
      case 3 => l3
      case 4 => l4
      case 5 => l5
      case 6 => l6
      case 7 => l7
      case 8 => l8
      case 9 => l9
      case 10 => l10
      case 11 => l11
      case 12 => l12
      case 13 => l13
      case 14 => l14
      case 15 => l15
      case 16 => l16
      case 17 => l17
      case 18 => l18
      case 19 => l19
      case 20 => l20
      case 21 => l21
      case 22 => l22
      case 23 => l23
      case 24 => l24
      case 25 => l25
      case 26 => l26
      case 27 => l27
      case 28 => l28
      case 29 => l29
      case 30 => l30
      case 31 => l31
    }
    arr.apply(elemId)

  }

  def getByIdR(idx: Int, sumLength: Int) = {
    val id = idx + 1
    val lId = Integer.numberOfLeadingZeros(id)
    val mask = -1 >>> lId
    val maskedSum = sumLength & mask

    var arrayId = 0
    var elemId = 0
    if (maskedSum < id) {
      arrayId = Integer.numberOfTrailingZeros(sumLength & ~mask)
      elemId = idx - maskedSum
      //println(s"1 id = $id, sumLength = $sumLength -> ($arrayId $elemId)")
    } else {
      arrayId = 31 - lId
      elemId = idx - (sumLength & (mask >>> 1))
      // println(s"2 id = $id, sumLength = $sumLength -> ($arrayId $elemId)")
    }
    val arr = (arrayId: @switch) match {
      case 0 => r0
      case 1 => r1
      case 2 => r2
      case 3 => r3
      case 4 => r4
      case 5 => r5
      case 6 => r6
      case 7 => r7
      case 8 => r8
      case 9 => r9
      case 10 => r10
      case 11 => r11
      case 12 => r12
      case 13 => r13
      case 14 => r14
      case 15 => r15
      case 16 => r16
      case 17 => r17
      case 18 => r18
      case 19 => r19
      case 20 => r20
      case 21 => r21
      case 22 => r22
      case 23 => r23
      case 24 => r24
      case 25 => r25
      case 26 => r26
      case 27 => r27
      case 28 => r28
      case 29 => r29
      case 30 => r30
      case 31 => r31
    }
    arr.apply(elemId)
  }

  final def applyf(idx: Int) = {
    if (idx < leftSize) getByIdL(idx, leftSize)
    else getByIdR(leftSize + rightSize - idx - 1, rightSize)
  }
  */

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

  def getById(idx: Int, sumLength: Int, arrays: Array[Array[Int]]): Int = {
    val id = idx + 1
    val lId = Integer.numberOfLeadingZeros(id)
    val mask = -1 >>> lId
    val maskedSum = sumLength & mask

    var elemId = 0

    val arr = if (maskedSum < id) {
      val arrayId = Integer.numberOfTrailingZeros(sumLength & ~mask) - HEADS_TAILS_BITS * 2 + 1
      elemId = idx - maskedSum
      //println(s"1 id = $id, sumLength = $sumLength -> ($arrayId $elemId)")

      arrays(arrayId)
    } else {
      val arrayId = 32 - HEADS_TAILS_BITS * 2 - lId
      elemId = idx - (sumLength & (mask >>> 1))
      // println(s"2 id = $id, sumLength = $sumLength -> ($arrayId $elemId)")
      arrays(arrayId)
    }

    var update = if(updates ne null) {
      history.coins += 1
      updates.get(arr)
    } else null
    while(update ne null) {
      update.coins += 1
      if (update.id == elemId) return update.newValue
      else update = update.next
    }

    arr(elemId)

  }

  def updateById(idx: Int, newElem:Int, sumLength: Int, arrays: Array[Array[Int]]): DArray = {
    if(idx < leftSize || idx > sumLength - rightSize) {
      val updId = if(idx < leftSize) leftSize - idx else HEAD_TAILS_SIZE - (idx - sumLength + rightSize)
      val narr = new Array[Int](HEAD_TAILS_SIZE)
      System.arraycopy(headsNTails,0, narr, 0, HEAD_TAILS_SIZE)
      narr(updId) = newElem
      new DArray(this.left, this.right, narr, headsSize, tailsSize, updates, this.history)
    } else {
      val id = idx + 1
      val lId = Integer.numberOfLeadingZeros(id)
      val mask = -1 >>> lId
      val maskedSum = sumLength & mask

      var elemId = 0

      val arr = if (maskedSum < id) {
        val arrayId = Integer.numberOfTrailingZeros(sumLength & ~mask)
        elemId = idx - maskedSum
        //println(s"1 id = $id, sumLength = $sumLength -> ($arrayId $elemId)")

        arrays(arrayId)
      } else {
        val arrayId = 31 - lId
        elemId = idx - (sumLength & (mask >>> 1))
        // println(s"2 id = $id, sumLength = $sumLength -> ($arrayId $elemId)")
        arrays(arrayId)
      }

      if (arr(elemId) == newElem) return this;

      var update = if (updates ne null) updates.get(arr) else null

      val newUpdate = new Update(arr, elemId, newElem, update, history)
      newUpdate.coins = if (update ne null) update.coins + 1 else 1
      val newUpdates =
        if (this.updates ne null)
          this.updates.clone().asInstanceOf[util.IdentityHashMap[Array[Int], Update]]
        else
          new util.IdentityHashMap[Array[Int], Update]()
      newUpdates.put(arr, newUpdate)
      new DArray(this.left, this.right, this.headsNTails, this.headsSize, this.tailsSize, newUpdates, newUpdate)
    }
  }

  final def updated(idx: Int, value: Int) = {
    if (idx < leftSize) updateById(idx,value, leftSize, left)
    else updateById(leftSize + rightSize - idx - 1, value, rightSize, right)
  }

  def getById1(id: Int, sumLength: Int, arrays: Array[Array[Int]]) = {
    val subst1 = if (id > 0) sumLength & (java.lang.Integer.highestOneBit(id) - 1) else 0
    var r = id - subst1          //  this trick gave 20% speedup
    var c = sumLength ^ subst1   //  todo: could we improve it?
    val subst2 = r & c
    r = r ^ subst2
    c = c ^ subst2
    var b = Integer.lowestOneBit(c)
    while (r >= b && r != 0) {
      r = r - b
      c = c ^ b
      b = Integer.lowestOneBit(c)
    }
    arrays(Integer.numberOfTrailingZeros(c))(r)
  }



  final def applyArr(idx: Int) = {
    if (idx < leftSize) getById(idx, leftSize, left)
    else getById(leftSize + rightSize - idx - 1, rightSize, right)
  }

  def apply(idx: Int) = {
    if (idx < headsSize)
      headsNTails(idx)
    else if(idx >= leftSize + rightSize + headsSize)
      headsNTails(ELEMS_HEAD + (leftSize + rightSize + headsSize + tailsSize - idx) - 1)
    else applyArr(idx - headsSize)
  }

  final def rebalance() =
    if (leftSize/ELEMS_FIRST_ARRAY > (rightSize/ELEMS_FIRST_ARRAY + 1) * 2 || rightSize/ELEMS_FIRST_ARRAY > (leftSize/ELEMS_FIRST_ARRAY + 1) * 2) {

      // determine new sizes
      val newRightSize = (rightSize / 2 / ELEMS_FIRST_ARRAY + leftSize / 2 / ELEMS_FIRST_ARRAY) * ELEMS_FIRST_ARRAY

      val newLeftSize = leftSize + rightSize - newRightSize
      val leftArraySize = larraySize(newLeftSize - 1) - HEADS_TAILS_BITS + 1
      val rightArraySize = larraySize(newRightSize - 1) - HEADS_TAILS_BITS + 1

      def rellocateNewArray(elemCount: Int, arraySize: Int): Array[Array[Int]] = {
        val arr = new Array[Array[Int]](arraySize)
        var i = elemCount / ELEMS_FIRST_ARRAY
        var j = 0
        var sz = ELEMS_FIRST_ARRAY
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

      // todo: incorporate updates

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
      // assignFields()
    }


  def append(el: Int): DArray = {
    if(tailsSize < ELEMS_HEAD) {
      val narr = new Array[Int](HEAD_TAILS_SIZE)
      System.arraycopy(headsNTails,0 ,narr, 0, ELEMS_HEAD)  //todo: 16 -> headsSize
      System.arraycopy(headsNTails, ELEMS_HEAD, narr, ELEMS_HEAD + 1, tailsSize)

      narr(ELEMS_HEAD) = el
      new DArray(this.left, this.right, narr, headsSize, tailsSize + 1, updates, this.history)
    } else {
      val newLeftSize = this.leftSize
      val newRightSize = this.rightSize + ELEMS_FIRST_ARRAY
      val newRightArraySize = larraySize(this.rightSize) + 1
      val rightArray = new Array[Array[Int]](newRightArraySize)

      var newArrayIdx = 0
      var sz = ELEMS_FIRST_ARRAY
      while ((newArrayIdx < right.length) && (right(newArrayIdx) ne null)) {
        newArrayIdx = newArrayIdx + 1
        sz = sz * 2
      }


      val newHeadsNTails = new Array[Int](HEAD_TAILS_SIZE)
      System.arraycopy(headsNTails, 0, newHeadsNTails, 0, ELEMS_HEAD) //todo: 16 -> headsSize
      System.arraycopy(headsNTails, ELEMS_HEAD, newHeadsNTails, ELEMS_HEAD + 1, ELEMS_FIRST_ARRAY)

      newHeadsNTails(ELEMS_HEAD) = el

      var rollingSourceArrayId = 0

      var rollingTargetElemId = ELEMS_FIRST_ARRAY
      val target = new Array[Int](sz)

      System.arraycopy(headsNTails, ELEMS_HEAD + ELEMS_FIRST_ARRAY, target, 0, ELEMS_FIRST_ARRAY)
      // todo: incorporate updates
      while (rollingSourceArrayId < newArrayIdx) {
        System.arraycopy(right(rollingSourceArrayId), 0, target, rollingTargetElemId, right(rollingSourceArrayId).length)
        rollingTargetElemId = rollingTargetElemId + right(rollingSourceArrayId).length
        rollingSourceArrayId = rollingSourceArrayId + 1
      }
      rightArray(newArrayIdx) = target


      if (newArrayIdx < right.length)
        System.arraycopy(right, newArrayIdx + 1, rightArray, newArrayIdx + 1, right.length - newArrayIdx - 1)

      val rez = new DArray(left, rightArray, newHeadsNTails, headsSize, ELEMS_FIRST_ARRAY + 1, updates, history)
      rez.rebalance()
      rez
    }
  }

  def prepend(el: Int): DArray = {
    if(headsSize < ELEMS_HEAD) {
      val narr = new Array[Int](HEAD_TAILS_SIZE)
      System.arraycopy(headsNTails,0 ,narr, 0, HEAD_TAILS_SIZE)  //todo: 16 -> headsSize

      narr(headsSize) = el
      new DArray(this.left, this.right, narr, headsSize + 1, tailsSize, updates, this.history)
    } else {
      val newLeftSize = this.leftSize + ELEMS_FIRST_ARRAY
      val newRightSize = this.rightSize
      val newLeftArrSize = larraySize(this.leftSize) + 1
      val leftArrays = new Array[Array[Int]](newLeftArrSize)

      var newArrayIdx = 0
      var sz = ELEMS_FIRST_ARRAY
      while ((newArrayIdx < right.length) && (right(newArrayIdx) ne null)) {
        newArrayIdx = newArrayIdx + 1
        sz = sz * 2
      }


      val newHeadsNTails = new Array[Int](HEAD_TAILS_SIZE)
      System.arraycopy(headsNTails, ELEMS_HEAD, newHeadsNTails, ELEMS_HEAD, ELEMS_HEAD) //todo: 16 -> headsSize
      System.arraycopy(headsNTails, 0, newHeadsNTails, 0, ELEMS_FIRST_ARRAY)

      newHeadsNTails(ELEMS_FIRST_ARRAY) = el

      var rollingSourceArrayId = 0

      var rollingTargetElemId = ELEMS_FIRST_ARRAY
      val target = new Array[Int](sz)

      System.arraycopy(headsNTails, ELEMS_HEAD + ELEMS_FIRST_ARRAY, target, 0, ELEMS_HEAD)
      // todo: incorporate updates
      while (rollingSourceArrayId < newArrayIdx) {
        System.arraycopy(left(rollingSourceArrayId), 0, target, rollingTargetElemId, left(rollingSourceArrayId).length)
        rollingTargetElemId = rollingTargetElemId + left(rollingSourceArrayId).length
        rollingSourceArrayId = rollingSourceArrayId + 1
      }
      leftArrays(newArrayIdx) = target


      if (newArrayIdx < right.length)
        System.arraycopy(left, newArrayIdx + 1, leftArrays, newArrayIdx + 1, left.length - newArrayIdx - 1)

      val rez = new DArray(leftArrays, right, newHeadsNTails, ELEMS_FIRST_ARRAY + 1, tailsSize, updates, history)
      rez.rebalance()
      rez
    }
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
      def advance(): Unit = {
        elemIdx = elemIdx + 1
        if (rollingSourceArrayId >= 0) {
          if (rollingSourceArrayId == left.length) {
            rollingSourceArrayId = -right.length
          } else if ((left(rollingSourceArrayId) eq null) || (elemIdx >= left(rollingSourceArrayId).length)) {
            rollingSourceArrayId = rollingSourceArrayId + 1
            elemIdx = -1
            advance()
          } else {} // done
        } else {
          if (rollingSourceArrayId == -1) {
            if (elemIdx > 0) () // finish
          } else if ((right(-rollingSourceArrayId - 1) eq null) || elemIdx >= right(-rollingSourceArrayId - 1).length) {
            rollingSourceArrayId = rollingSourceArrayId + 1
            elemIdx = -1
            advance()
          } else {} // done
        }
      }

      def hasNext: Boolean = {
        if (elemIdx == -1) advance()
        !(rollingSourceArrayId == -1 && ((right(0) eq null) || elemIdx > 0))
      }


      def next(): Int = {
        if (!hasNext) ???
        else {
          val r =
            if (rollingSourceArrayId >= 0)
              left(rollingSourceArrayId)(elemIdx)
            else right(-rollingSourceArrayId - 1)(right(-rollingSourceArrayId - 1).length - 1  - elemIdx)
          advance()
          r
        }
      }
    }
    t
  }

}


object DArray{
  final val HEADS_TAILS_BITS = 1
  final val HEAD_TAILS_SIZE = 1 << ( 2 * HEADS_TAILS_BITS + 1)
  final val ELEMS_HEAD = HEAD_TAILS_SIZE >> 1
  final val ELEMS_TAIL = HEAD_TAILS_SIZE >> 1
  final val ELEMS_FIRST_ARRAY = HEAD_TAILS_SIZE >> 2
  val empty = new DArray(Array.empty, Array.empty, new Array[Int](HEAD_TAILS_SIZE), 0, 0)
  def apply(elems: Int*) = { //todo: rewrite
    var t = empty
    val heads = elems.take(ELEMS_HEAD - 1)
    for(elem <- heads)
      t = t.prepend(elem)
    for(elem <- elems.drop(ELEMS_HEAD - 1)) {
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

  class Update(val arr: Array[Int], val id: Int, val newValue: Int, val next: Update, val history: Update) {
    var coins = 0
    var newArr: Array[Int] = null
  }
}
