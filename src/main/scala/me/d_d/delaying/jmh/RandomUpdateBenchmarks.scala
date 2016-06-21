package me.d_d.delaying.jmh

import me.d_d.delaying.{DelayingArray, IntRRBVector}
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.results.format.{ResultFormat, ResultFormatType}
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.{CommandLineOptions, Options, OptionsBuilder}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
class UpdateRandomBenchmarks {

  @Param(Array("10000", "100000", "1000000", "10000000", "40000000"))
  var size: Int = _

  var arr: Array[Int] = _
  var darr: DelayingArray = _
  var rrb_vec: IntRRBVector = _
  var vec: Vector[Int] = _

  @Setup
  def setup() = {
    val range = 0 until size
    arr = Array(range: _*)
    darr = DelayingArray(range: _*)
    rrb_vec = IntRRBVector(range: _*)
    vec = Vector(range: _*)
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

  @Benchmark
  def ArrayUpdate(bh: Blackhole): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(nextInt(size)) = i
      i += 1
    }

    i = 0
    while (i < arr.size) {
      bh.consume(arr(i))
      i += 1
    }
    i = 0
    while (i < arr.size) {
      bh.consume(arr(i))
      i += 1
    }
  }

  @Benchmark
  def DelayingArrayUpdate(bh: Blackhole): Unit = {
    var i = 0
    while (i < darr.size) {
      darr = darr.updated(nextInt(size), i)
      i += 1
    }

    i = 0
    while (i < darr.size) {
      bh.consume(darr(i))
      i += 1
    }
    i = 0
    while (i < darr.size) {
      bh.consume(darr(i))
      i += 1
    }
  }

  @Benchmark
  def VectorForeachUpdate(bh: Blackhole): Unit = {
    var i = 0
    while (i < vec.size) {
      vec = vec.updated(nextInt(size), i)
      i += 1
    }

    i = 0
    while (i < vec.size) {
      bh.consume(vec(i))
      i += 1
    }
    i = 0
    while (i < vec.size) {
      bh.consume(vec(i))
      i += 1
    }
  }

  @Benchmark
  def IntRRBVectorUpdate(bh: Blackhole): Unit = {
    var i = 0
    while (i < rrb_vec.length) {
      rrb_vec = rrb_vec.updated(nextInt(size), i)
      i += 1
    }

    i = 0
    while (i < rrb_vec.length) {
      bh.consume(rrb_vec(i))
      i += 1
    }
    i = 0
    while (i < rrb_vec.length) {
      bh.consume(rrb_vec(i))
      i += 1
    }
  }
}

object UpdateRandomBenchmarks {

  import scala.collection.JavaConversions._

  def main(args: Array[String]): Unit = {
    //val cmdLine = new CommandLineOptions(args: _*)

    val defaults = new OptionsBuilder()
      .include(classOf[UpdateRandomBenchmarks].getSimpleName)
      .mode(Mode.AverageTime)
      .resultFormat(ResultFormatType.CSV)
      .result("update_read_random.csv")
      .warmupIterations(10)
      .measurementIterations(10)
      .timeUnit(TimeUnit.MILLISECONDS)
      .jvmArgs("-server")
      .jvmArgs("-Xmx6G")
      .threads(1)
      .forks(1)
      .build()

    val runner = new Runner(defaults)
    val results = runner.run()
  }
}
