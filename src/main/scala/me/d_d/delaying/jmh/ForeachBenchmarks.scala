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
class ForeachBenchmarks {

  @Param(Array("10000", "100000", "1000000", "10000000", "30000000", "50000000", "80000000"))
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
  }

  @Benchmark
  def ArrayForeach(bh: Blackhole): Unit = {
    arr.foreach(bh.consume)
    /*var i = 0
    val len = arr.length
    while (i < len) { bh.consume(arr.apply(i)); i += 1 }*/
  }

  @Benchmark
  def DelayingArrayForeach(bh: Blackhole): Unit = {
    darr.foreach(bh.consume)
  }

  @Benchmark
  def VectorForeach(bh: Blackhole): Unit = {
    vec.foreach(bh.consume)
  }

  @Benchmark
  def IntRRBVectorForeach(bh: Blackhole): Unit = {
    rrb_vec.foreach(bh.consume)
  }
}

object ForeachBenchmarks {

  import scala.collection.JavaConversions._

  def main(args: Array[String]): Unit = {
    //val cmdLine = new CommandLineOptions(args: _*)

    val defaults = new OptionsBuilder()
      .include(classOf[ForeachBenchmarks].getSimpleName)
      .mode(Mode.AverageTime)
      .resultFormat(ResultFormatType.CSV)
      .result("foreach_big_single.csv")
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
