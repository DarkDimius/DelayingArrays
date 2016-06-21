package me.d_d.delaying.jmh

import java.util.concurrent.TimeUnit

import me.d_d.delaying.{DArray, DelayingArray, IntRRBVector}
import org.openjdk.jmh.annotations.{Mode, OperationsPerInvocation, _}
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.results.format.ResultFormatType
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.{CommandLineOptions, OptionsBuilder}

@State(Scope.Benchmark)
class AppendBenchmarks {

  @Param(Array("10000", "100000", "1000000", "10000000", "30000000", "50000000", "80000000"))
  var size: Int = _

  var range: Range = _

  @Setup
  def setup() = {
    range = 0 until size
  }

  @Benchmark
  def DelayingArrayAppend(bh: Blackhole): Unit = {
    val result = range.foldLeft(DelayingArray.empty)(_ append _)
    bh.consume(result)
  }

  @Benchmark
  def IntRRBVectorAppend(bh: Blackhole): Unit = {
    val result = range.foldLeft(IntRRBVector.empty)(_ :+ _)
    bh.consume(result)
  }

  @Benchmark
  def VectorAppend(bh: Blackhole): Unit = {
    val result = range.foldLeft(Vector[Int]())(_ :+ _)
    bh.consume(result)
  }
}

object AppendBenchmarks {
  import scala.collection.JavaConversions._

  def main(args: Array[String]): Unit = {
    //val cmdLine = new CommandLineOptions(args: _*)

    val defaults = new OptionsBuilder()
      .include(classOf[AppendBenchmarks].getSimpleName)
      .mode(Mode.AverageTime)
      .resultFormat(ResultFormatType.CSV)
      .result("append_big_fix.csv")
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
