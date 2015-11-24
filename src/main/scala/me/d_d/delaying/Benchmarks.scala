package me.d_d.delaying

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

/**
  * Created by dark on 23/11/15.
  */
@State(Scope.Benchmark)
class Benchmarks {

  /*
[info] Benchmark                         Mode  Cnt       Score        Error  Units
[info] Benchmarks.arr_apply              avgt   30      13.495 ±      5.990  ns/op
[info] Benchmarks.darr_apply             avgt   30      22.800 ±      6.680  ns/op
[info] Benchmarks.vec_apply              avgt   30      28.919 ±     38.852  ns/op

[info] Benchmarks.arr_foreach            avgt   30  423603.350 ± 239141.019  ns/op
[info] Benchmarks.darr_foreach           avgt   30  240191.217 ±  51075.970  ns/op
[info] Benchmarks.vec_foreach            avgt   30  299815.300 ±  74171.103  ns/op

[info] Benchmarks.arr_iterator_foreach   avgt   30  448778.639 ± 176362.034  ns/op
[info] Benchmarks.darr_iterator_foreach  avgt   30  643554.768 ±  95530.380  ns/op
[info] Benchmarks.vec_iterator_foreach   avgt   30  306579.976 ± 324088.290  ns/op
    */

  // as can be seen from numbers above:
  // 1: for apply: faster then vector, and substantially less deviation
  //      profiles suggest that hot place is calculation of indexes in arr_apply
  //      I was already able to speed it up by 20% by adding more byte-shift a trick there
  //      it would be good to see if some more tricks could be done to reduce number of iterations
  //      would be even better to find a way to not have a cycle in the first place
  //
  // 2: for foreach and iterator: reboxing spoils the picture


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
