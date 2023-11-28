package org.enso.languageserver.profiling

import org.enso.profiling.snapshot.ProfilingSnapshot

import java.nio.file.{Files, Path}

/** Generating a memory dump can take significant time. This test profiling snapshot
  * just creates an empty file in the specified location.
  */
final class TestProfilingSnapshot extends ProfilingSnapshot {

  /** @inheritdoc */
  override def generateSnapshot(output: Path): Unit = {
    Files.write(output, Array.emptyByteArray)
  }
}
