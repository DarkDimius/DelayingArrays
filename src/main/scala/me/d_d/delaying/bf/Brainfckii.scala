package me.d_d.delaying.bf

import me.d_d.delaying.DelayingArray

import annotation.tailrec

/**
  * Functional Brainf*ck interpreter (hackerrank)
  * The original idea was to implement a debugger, persisting
  * the state every 10^6 operations, being able to go back
  * and replay the program on demand, while keeping acceptable performance.
  *
  * A DelayingArray is used for the persistent 'tape'
  * Sadly Vector outperforms Delaying array, the mixed reads and updates never
  * let the Delaying Array to settle, the list traversal overhead is to blame
  */
object Brainfckii {
  sealed abstract class Op

  sealed class State(val opnum: Int, val ix: Int,
                     val mem: DelayingArray,
                     val stdin: List[Int])

  val maxIters = 100000000000L

  @tailrec
  def eval(program: List[Op], state: State, inner: Boolean): (Boolean, State) =
    program match {
      case Nil => {
        if (inner)
          (state.mem(state.ix) != 0, state)
        else
          (false, state)
      }
      case (op :: ops) => {
        val newState = evalOp(op, state)
        eval(ops, newState, inner)
      }
    }

  @tailrec
  def evalOp(op: Op, state: State): State = {
    if (state.opnum >= maxIters) {
      println("\nPROCESS TIME OUT. KILLED!!!")
      System.exit(0)
      state
    } else op match {
      case IncPtr => new State(state.opnum + 1, state.ix + 1, state.mem, state.stdin)
      case DecPtr => new State(state.opnum + 1, state.ix - 1, state.mem, state.stdin)
      case IncByte => new State(state.opnum + 1, state.ix,
        state.mem.updated(state.ix, (state.mem(state.ix) + 1) & 255),
        state.stdin)
      case DecByte => new State(state.opnum + 1, state.ix,
        state.mem.updated(state.ix, (state.mem(state.ix) + 255) & 255),
        state.stdin)
      case Outp => {
        print(state.mem(state.ix).toChar)
        new State(state.opnum + 1, state.ix, state.mem, state.stdin)
      }
      case Inp => new State(state.opnum + 1, state.ix,
        state.mem.updated(state.ix, state.stdin.head.toInt),
        state.stdin.tail)
      case Loop(body) => {
        if (state.mem(state.ix) == 0)
          new State(state.opnum + 2, state.ix, state.mem, state.stdin)
        else {
          val (repeat, newState) = eval(body, new State(state.opnum + 1,
            state.ix, state.mem, state.stdin), true)
          if (repeat)
            evalOp(op, newState)
          else
            newState
        }
      }
      case Pool => new State(state.opnum + 1, state.ix, state.mem, state.stdin)
    }
  }

  def parse(code: String): List[Op] = {
    @tailrec
    def aux(code: String, acc: List[Op]): (List[Op], String) = {
      if (code.isEmpty)
        (acc.reverse, code)
      else
        code.head match {
          case '>' => aux(code.tail, IncPtr :: acc)
          case '<' => aux(code.tail, DecPtr :: acc)
          case '+' => aux(code.tail, IncByte :: acc)
          case '-' => aux(code.tail, DecByte :: acc)
          case '.' => aux(code.tail, Outp :: acc)
          case ',' => aux(code.tail, Inp :: acc)
          case '[' => {
            val (ops, rem) = aux2(code.tail, Nil)
            aux(rem, Loop(ops) :: acc)
          }
          case ']' => ((Pool :: acc).reverse, code.tail)
          case _ => aux(code.tail, acc)
        }
    }
    def aux2(code: String, acc: List[Op]) = aux(code, acc)
    aux(code, Nil)._1
  }


  case object IncPtr extends Op
  case object DecPtr extends Op
  case object IncByte extends Op
  case object DecByte extends Op
  case object Outp extends Op
  case object Inp extends Op
  case class Loop(val body: List[Op]) extends Op
  case object Pool extends Op


  def main(args: Array[String]) {
    //val l = readLine.split(" ").map(_.toInt)
    val stdin = readLine.toList.map(_.toInt)
    val program = readLine
    val ast = parse(program)

    val ticks = System.currentTimeMillis()
    eval(ast, new State(0, 0,
      DelayingArray(0 until 262143 map ( _ => 0) : _*), stdin), false)

    println(s"Done in ${System.currentTimeMillis() - ticks} ms")
  }
}

