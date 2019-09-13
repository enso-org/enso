package org.enso.interpreter.bench

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.JavaConverters._

class RegressionTest extends FlatSpec with Matchers {
  // This tolerance may be adjusted depending on the stability of CI
  final val TOLERANCE = 0.2

  val runner     = new BenchmarksRunner
  val benchmarks = List(runner.getAvailable.asScala: _*)

  benchmarks.foreach { benchmark =>
    benchmark should "not be slower than before" in {
      val item       = runner.run(benchmark)
      val difference = (item.getScore / item.getBestScore) - 1
      println(s"Difference was: $difference.")
      difference should be < TOLERANCE
    }
  }
}
