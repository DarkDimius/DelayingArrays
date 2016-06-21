package me.d_d.delaying.jmh

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@State(Scope.Benchmark)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
abstract class SequentialApply(generator: ThingGenerator) {
  @Param(Array("1234567"))
  var size: Int = _
  var thing: ThingWithApply = _

  @Setup
  def setup(): Unit = {
    thing = generator.apply((0 until size): _*)
  }

  @Benchmark
  def sequentialApply(bh: Blackhole): Unit = {
    var i = 0
    var sum = 0
    while (i < size) {
      sum += thing.apply(i)
      i += 1
    }
    bh.consume(sum)
  }
}

trait ThingWithApply {
  def apply(index: Int): Int
}

trait ThingGenerator {
  def apply(elems: Int*): ThingWithApply
}

object ArrayGenerator extends ThingGenerator {
  override def apply(elems: Int*): ThingWithApply = {
    new ThingWithApply {
      val arr = Array(elems: _*)
      override def apply(index: Int): Int = arr.apply(index)
    }
  }
}

object VectorGenerator extends ThingGenerator {
  override def apply(elems: Int*): ThingWithApply = {
    new ThingWithApply {
      val vec = Vector(elems: _*)
      override def apply(index: Int): Int = vec.apply(index)
    }
  }
}

object Runner {
  class ArraySequentialApply extends SequentialApply(ArrayGenerator)
  class IntRRBVectorSequentialApply extends SequentialApply(VectorGenerator)
}
