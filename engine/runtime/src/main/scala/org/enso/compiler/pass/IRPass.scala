package org.enso.compiler.pass

import org.enso.compiler.core.IR
import org.enso.interpreter.runtime.scope.{LocalScope, ModuleScope}

/** A representation of a compiler pass that runs on the [[IR]] type. */
trait IRPass {

  /** The type of the metadata object that the pass writes to the IR. */
  type Metadata <: IR.Metadata

  /** Executes the pass on the provided `ir`, and returns a possibly transformed
    * or annotated version of `ir`.
    *
    * @param ir the Enso IR to process
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  def runModule(ir: IR.Module): IR.Module

  /** Executes the pass on the provided `ir`, and returns a possibly transformed
    * or annotated version of `ir` in an inline context.
    *
    * @param ir the Enso IR to process
    * @param localScope the local scope in which the expression is executed
    * @param moduleScope the module scope in which the expression is executed
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  def runExpression(
    ir: IR.Expression,
    localScope: Option[LocalScope]   = None,
    moduleScope: Option[ModuleScope] = None
  ): IR.Expression
}
