import java.io.InputStreamReader
import scala.annotation.tailrec
import scala.io.Source
//import scala.util.parsing.combinator._
//import scala.util.parsing.input.CharArrayReader
/*
class ProgStatus(pops: Int, pleft: List[Int], pcur: Int, pright: List[Int]) {
  val (ops, left, cur, right) = (pops, pleft, pcur, pright)
}

class Reg extends RegexParsers {
  val lines = io.Source.stdin.getLines()
  val n :: m :: tailline = lines.next.split(" ").map( _.toLong ).toList
  val input = lines.next.dropRight(1).toCharArray.iterator

  def fail(s:ProgStatus): ProgStatus = {
    println("")
    print("PROCESS TIME OUT. KILLED!!!")
    sys.exit(0)
    s
  }

  def checkOps(fun: (ProgStatus => ProgStatus)) : ProgStatus => ProgStatus = {
    a => a match {
      case a:ProgStatus if a.ops >= 100000 => fail(a)
      case _ => fun(a)
    }
  }


  final def doBrack(content: (ProgStatus => ProgStatus)): ProgStatus => ProgStatus = {
    @tailrec def doBrackInner(prev:ProgStatus):ProgStatus = prev.cur match {
      case 0 => new ProgStatus(prev.ops + 1, prev.left, prev.cur, prev.right)
      case _ => doBrackInner(checkOps(content)(new ProgStatus(prev.ops + 2, prev.left, prev.cur, prev.right)))
    }
    prev => prev.cur match {
      case 0 => new ProgStatus(prev.ops + 2, prev.left, prev.cur, prev.right)
      case _ => doBrackInner(new ProgStatus(prev.ops -1 , prev.left, prev.cur, prev.right))
    }
  }


  def doOps(ops: List[ProgStatus => ProgStatus]):ProgStatus => ProgStatus = ops match {
    case Nil => (x => x)
    case _ => ops.reduce((a,b) => (x => b(a(x))))
  }

  def doInc(prev: ProgStatus):ProgStatus =  {
    new ProgStatus(prev.ops + 1, prev.left, (prev.cur+1)%256, prev.right)
  }

  def doDec(prev: ProgStatus):ProgStatus = {
    new ProgStatus(prev.ops + 1, prev.left, (prev.cur+255)%256, prev.right)
  }

  def doL(prev: ProgStatus):ProgStatus = prev.left match {
    case Nil => fail(prev)
    case head :: tail => new ProgStatus(prev.ops+1, tail, head, prev.cur :: prev.right)
  }

  def doR(prev: ProgStatus):ProgStatus = prev.right match {
    case Nil => new ProgStatus(prev.ops + 1, prev.cur :: prev.left, 0, Nil)
    case head :: tail => new ProgStatus(prev.ops+1, prev.cur :: prev.left, head, tail)
  }

  def doInp(prev: ProgStatus):ProgStatus = {
    new ProgStatus(prev.ops + 1, prev.left, input.next, prev.right)
  }

  def doOutp(prev: ProgStatus):ProgStatus = {
    Console.out.write(prev.cur)
    new ProgStatus(prev.ops + 1, prev.left, prev.cur, prev.right)
  }

  def brack: Parser[ProgStatus => ProgStatus] = "["~ops~"]" ^^ { case a ~ x ~ c => doBrack(x)}
  def ops: Parser[ProgStatus => ProgStatus] = rep(inp | outp | dec | inc | l | r | brack | nop) ^^ doOps
  def inp: Parser[ProgStatus => ProgStatus] = "," ^^ (_ => checkOps(doInp))
  def outp: Parser[ProgStatus => ProgStatus] = "." ^^ (_ => checkOps(doOutp))
  def dec: Parser[ProgStatus => ProgStatus] = "-" ^^ (_ => checkOps(doDec))
  def inc: Parser[ProgStatus => ProgStatus] = "+" ^^ (_ => checkOps(doInc))
  def l: Parser[ProgStatus => ProgStatus] = ">" ^^ (_ => checkOps(doR))
  def r: Parser[ProgStatus => ProgStatus] = "<" ^^ (_ => checkOps(doL))
  def nop: Parser[ProgStatus => ProgStatus] = """[^,\.\-+<>\[\]]+""".r ^^ (_ => (x => x))
}


object Solution extends Reg {
  def main(args: Array[String]) {
    val chars = lines.take(m.toInt).reduce(_+_)
    val parsed = parseAll(ops, chars).get
    parsed(new ProgStatus(0,Nil,0,Nil))
    println("")
  }
}
*/