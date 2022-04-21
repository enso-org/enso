package org.enso.compiler

import org.enso.interpreter.runtime.Module

/** The result of running the compiler.
  *
  * @param compiledModules the modules compiled during the run
  */
final case class CompilerResult(compiledModules: List[Module])
object CompilerResult {

  /** @return an empty compiler result */
  def empty: CompilerResult =
    CompilerResult(Nil)
}
