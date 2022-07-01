package org.enso.interpreter.instrument.job

import org.enso.compiler.CompilerResult
import org.enso.compiler.context.Changeset
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.execution.model.PendingEdit
import org.enso.interpreter.runtime.Module
import org.enso.text.buffer.Rope

import java.io.File
import java.util.logging.Level

final class ApplyPendingEditsJob extends Job[Unit](List.empty, true, false) {

  import ApplyPendingEditsJob._

  override def run(implicit ctx: RuntimeContext): Unit = {
    ctx.locking.acquirePendingEditsLock()
    ctx.locking.acquireWriteCompilationLock()
    try {
      val pendingEdits  = ctx.state.pendingEdits.dequeueAll()
      val resolvedEdits = resolveFiles(pendingEdits)
      val modules       = resolvedEdits.keys
      ensureCompiled(modules)
      val applyEditsResults = applyEdits(resolvedEdits)
      invalidateCaches(applyEditsResults)
      compile(applyEditsResults)
      compileScope()
    } finally {
      ctx.locking.releaseWriteCompilationLock()
      ctx.locking.releasePendingEditsLock()
    }
  }

}
object ApplyPendingEditsJob {

  case class ApplyEditsResult(
    module: Module,
    changeset: Changeset[Rope],
    shouldRecompile: Boolean
  )

  private def resolveFiles(edits: Map[File, Seq[PendingEdit]])(implicit
    ctx: RuntimeContext
  ): Map[Module, Seq[PendingEdit]] = {
    edits.flatMap { case (file, edits) =>
      val moduleOpt = ctx.executionService.getContext.getModuleForFile(file)
      if (moduleOpt.isEmpty) None
      else Some(moduleOpt.get() -> edits)
    }
  }

  private def applyEdits(edits: Iterable[(Module, Seq[PendingEdit])])(implicit
    ctx: RuntimeContext
  ): Iterable[ApplyEditsResult] =
    edits.map(Function.tupled(applyEdits))

  private def applyEdits(module: Module, edits: Seq[PendingEdit])(implicit
    ctx: RuntimeContext
  ): ApplyEditsResult = {
    val file = new File(module.getPath)
    ctx.locking.acquireFileLock(file)
    try {
      ctx.executionService.modifyModuleSources(file, edits.map(_.edit), null)
      val shouldRecompile = edits.nonEmpty && edits.exists(_.execute)
      ApplyEditsResult(module, null, shouldRecompile)
    } finally ctx.locking.releaseFileLock(file)

  }

  private def invalidateCaches(editResults: Iterable[ApplyEditsResult])(implicit
    ctx: RuntimeContext
  ): Unit = ???

  private def ensureCompiled(modules: Iterable[Module])(implicit
    ctx: RuntimeContext
  ): Iterable[CompilerResult] =
    modules.map(ensureCompiled)

  private def ensureCompiled(
    module: Module
  )(implicit ctx: RuntimeContext): CompilerResult =
    if (
      !module.getCompilationStage.isAtLeast(
        Module.CompilationStage.AFTER_CODEGEN
      )
    ) {
      ctx.executionService.getLogger
        .log(Level.FINEST, s"Compiling ${module.getName}.")
      val result = ctx.executionService.getContext.getCompiler.run(module)
      result.copy(compiledModules =
        result.compiledModules.filter(_.getName != module.getName)
      )
    } else {
      CompilerResult.empty
    }

  private def compile(editResults: Iterable[ApplyEditsResult])(implicit
    ctx: RuntimeContext
  ): Unit = ???

  private def compileScope()(implicit ctx: RuntimeContext): Unit = ???
}
