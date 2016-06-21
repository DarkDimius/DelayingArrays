package me.d_d.delaying.jmh

import me.d_d.delaying.{DelayingArray, IntRRBVector}
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.results.format.{ResultFormat, ResultFormatType}
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.{CommandLineOptions, Options, OptionsBuilder}

@State(Scope.Benchmark)
class IteratorBenchmarks {

  @Param(Array("16777215"))
  var size: Int = _

  var arr: Array[Int] = _
  var darr: DelayingArray = _
  var rrb_vec: IntRRBVector = _

  @Setup
  def setup() = {
    val range = 0 until size
    arr = Array(range: _*)
    darr = DelayingArray(range: _*)
    rrb_vec = IntRRBVector(range: _*)
  }

  @Benchmark
  def ArrayIterator(bh: Blackhole): Unit = arr.iterator.foreach(bh.consume)

  @Benchmark
  def DArrayIterator(bh: Blackhole): Unit = darr.iterator.foreach(bh.consume)

  @Benchmark
  def RRBVectorIterator(bh: Blackhole): Unit = rrb_vec.iterator.foreach(bh.consume)
}

object IteratorBenchmarks {

  import scala.collection.JavaConversions._

  def main(args: Array[String]): Unit = {
    val cmdLine = new CommandLineOptions(args: _*)

    val defaults = new OptionsBuilder()
      .include(classOf[IteratorBenchmarks].getSimpleName)
      .mode(Mode.AverageTime)
      .resultFormat(ResultFormatType.JSON)
      .result("benchmarks.json")
      .warmupIterations(5)
      .measurementIterations(5)
      .timeUnit(TimeUnit.MILLISECONDS)
      .jvmArgs("-server")
      .threads(1)
      .forks(1)
      .build()

    val runner = new Runner(defaults)
    val results = runner.run()
  }
}
