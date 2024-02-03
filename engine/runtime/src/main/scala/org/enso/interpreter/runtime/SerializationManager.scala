package org.enso.interpreter.runtime

import org.enso.compiler.Compiler
import org.enso.compiler.context.CompilerContext

final class SerializationManager(private val context: TruffleCompilerContext) {

  def this(compiler: Compiler) = {
    this(compiler.context.asInstanceOf[TruffleCompilerContext])
  }

  private val pool = new SerializationPool(context)

  def getPool(): SerializationPool = pool

  // Make sure it is started to avoid races with language shutdown with low job
  // count.
  if (context.isCreateThreadAllowed) {
    pool.prestartAllCoreThreads()
  }

  // === Interface ============================================================

  def shutdown(waitForPendingJobCompletion: Boolean = false): Unit =
    pool.shutdown(waitForPendingJobCompletion)
}

object SerializationManager {
  def apply(context: CompilerContext): SerializationManager = {
    context.asInstanceOf[TruffleCompilerContext].getSerializationManager()
  }
}
