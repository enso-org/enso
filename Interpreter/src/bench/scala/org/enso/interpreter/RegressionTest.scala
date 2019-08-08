package org.enso.interpreter

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import collection.JavaConverters._

class RegressionTest extends FlatSpec with Matchers {
  final val TOLERANCE = 0.2

  val runner     = new BenchmarksRunner
  val benchmarks = List(runner.getAvailable.asScala: _*)

  benchmarks.foreach { benchmark =>
    benchmark should "not be slower than before" in {
      val item       = runner.run(benchmark)
      val difference = (item.getScore / item.getBestScore) - 1
      difference should be < TOLERANCE
    }
  }
}
