package me.d_d.delaying.jmh

import me.d_d.delaying.{DelayingArray, IntRRBVector}
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.results.format.{ResultFormat, ResultFormatType}
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.{CommandLineOptions, Options, OptionsBuilder}

@State(Scope.Benchmark)
class RandomApplyBenchmarks {

  final val iterations = 1000000

  @Param(Array("10000", "100000", "1000000", "10000000", "30000000", "50000000", "80000000"))
  var size: Int = _

  var arr: Array[Int] = _
  var darr: DelayingArray = _
  var rrb_vec: IntRRBVector = _
  var vec: Vector[Int] = _

  @Setup
  def setup() = {
    arr = Array((1 to size): _*)
    darr = DelayingArray((1 to size): _*)
    rrb_vec = IntRRBVector((1 to size): _*)
    vec = Vector((1 to size): _*)
    seedi = 1055984764
  }

  var seedi: Int = _

  // https://en.wikipedia.org/wiki/MINSTD
  def nextInt(): Int = {
    seedi = seedi * 48271
    seedi
  }

  def nextInt(bound: Int): Int = {
    val r = nextInt() % bound
    if (r >= 0)
      r
    else r + bound
  }
/*
  @Benchmark
  @OperationsPerInvocation(iterations)
  def ArrayRandomApply(bh: Blackhole): Unit = {
    var i = 0
    while(i < iterations) {
      bh.consume(arr.apply(nextInt(size)))
      i += 1
    }
  }*/

  @Benchmark
  @OperationsPerInvocation(iterations)
  def DelayingArrayRandomApply(bh: Blackhole): Unit = {
    var i = 0
    while(i < iterations) {
      bh.consume(darr.apply(nextInt(size)))
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(iterations)
  def IntRRBVectorRandomApply(bh: Blackhole): Unit = {
    var i = 0
    while (i < iterations) {
      bh.consume(rrb_vec.apply(nextInt(size)))
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(iterations)
  def VectorRandomApply(bh: Blackhole): Unit = {
    var i = 0
    while(i < iterations) {
      bh.consume(vec.apply(nextInt(size)))
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(iterations)
  def BaselineRandomApply(bh: Blackhole): Unit = {
    var i = 0
    while(i < iterations) {
      bh.consume(nextInt(size))
      i += 1
    }
  }
}

object RandomApplyBenchmarks {
  import scala.collection.JavaConversions._

  def main(args: Array[String]): Unit = {
    //val cmdLine = new CommandLineOptions(args: _*)

    val defaults = new OptionsBuilder()
      .include(classOf[RandomApplyBenchmarks].getSimpleName)
      .mode(Mode.AverageTime)
      .resultFormat(ResultFormatType.CSV)
      .result("apply_random_big_fix.csv")
      .warmupIterations(10)
      .measurementIterations(10)
      .timeUnit(TimeUnit.NANOSECONDS)
      .jvmArgs("-server")
      .jvmArgs("-Xmx6G")
      .threads(1)
      .forks(1)
      .build()

    //val opts = cmdLine.getIncludes.foldLeft(defaults)(_ include _).build()

    val runner = new Runner(defaults)
    val results = runner.run()
  }
}
