package org.enso.interpreter.instrument.job

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.EnsureCompiledJob.CompilationStatus

import java.io.File

class SlowEnsureCompiledJob(
  files: Iterable[File],
  isCancellable: Boolean = true
) extends EnsureCompiledJob(files, isCancellable) {

  override def runImpl(implicit ctx: RuntimeContext): CompilationStatus = {
    Thread.sleep(1000)
    super.run(ctx)
  }

}
