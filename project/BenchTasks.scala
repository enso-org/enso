package org.enso.build

import sbt.inputKey
import sbt.taskKey

/**
  * Defines benchmarking related task keys.
  */
object BenchTasks {
  lazy val bench     = taskKey[Unit]("Run Benchmarks")
  lazy val benchOnly = inputKey[Unit]("Run benchmarks by name substring")
}
