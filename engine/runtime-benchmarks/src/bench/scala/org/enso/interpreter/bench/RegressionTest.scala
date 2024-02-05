package org.enso.interpreter.bench

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters._
import scala.math.Ordering.Double.IeeeOrdering

class RegressionTest extends AnyFlatSpec with Matchers {
  // This tolerance may be adjusted depending on the stability of CI
  final val TOLERANCE = 0.2

  val runner     = new BenchmarksRunner
  val benchmarks = runner.getAvailable.asScala

  benchmarks.foreach { benchmark =>
    benchmark should "not be slower than before" in {
      val item       = runner.run(benchmark)
      val difference = (item.getScore / item.getBestScore) - 1
      println(s"Difference was: $difference.")
      difference should be < TOLERANCE
    }
  }
}
