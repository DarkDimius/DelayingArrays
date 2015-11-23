package me.d_d.delaying

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

/**
  * Created by dark on 23/11/15.
  */
@State(Scope.Benchmark)
class Benchmarks {


  val size = 73121
  val darr = DArray(1 to size :_*)
  val arr = Array(1 to size :_*)
  val vec = Vector(1 to size : _*)
  val rnd = new java.util.Random(42)


  @Benchmark
  @BenchmarkMode(Array(/*Mode.Throughput, */Mode.AverageTime/*, Mode.SampleTime, Mode.SingleShotTime*/))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def darr_apply(bh: Blackhole) = darr.apply(rnd.nextInt(size))

  @Benchmark
  @BenchmarkMode(Array(/*Mode.Throughput, */Mode.AverageTime/*, Mode.SampleTime, Mode.SingleShotTime*/))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def arr_apply(bh: Blackhole) = arr.apply(rnd.nextInt(size))

  @Benchmark
  @BenchmarkMode(Array(/*Mode.Throughput, */Mode.AverageTime/*, Mode.SampleTime, Mode.SingleShotTime*/))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def vec_apply(bh: Blackhole) = vec.apply(rnd.nextInt(size))

  @Benchmark
  @BenchmarkMode(Array(/*Mode.Throughput, */Mode.AverageTime/*, Mode.SampleTime, Mode.SingleShotTime*/))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def darr_foreach(bh: Blackhole) = darr.foreach(bh.consume)

  @Benchmark
  @BenchmarkMode(Array(/*Mode.Throughput, */Mode.AverageTime/*, Mode.SampleTime, Mode.SingleShotTime*/))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def arr_foreach(bh: Blackhole) = arr.foreach(bh.consume)

  @Benchmark
  @BenchmarkMode(Array(/*Mode.Throughput, */Mode.AverageTime/*, Mode.SampleTime, Mode.SingleShotTime*/))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def vec_foreach(bh: Blackhole) = vec.foreach(bh.consume)

  @Benchmark
  @BenchmarkMode(Array(/*Mode.Throughput, */Mode.AverageTime/*, Mode.SampleTime, Mode.SingleShotTime*/))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def darr_iterator_foreach(bh: Blackhole) = darr.iterator.foreach(bh.consume)

  @Benchmark
  @BenchmarkMode(Array(/*Mode.Throughput, */Mode.AverageTime/*, Mode.SampleTime, Mode.SingleShotTime*/))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def arr_iterator_foreach(bh: Blackhole) = arr.iterator.foreach(bh.consume)

  @Benchmark
  @BenchmarkMode(Array(/*Mode.Throughput, */Mode.AverageTime/*, Mode.SampleTime, Mode.SingleShotTime*/))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def vec_iterator_foreach(bh: Blackhole) = vec.iterator.foreach(bh.consume)

}
