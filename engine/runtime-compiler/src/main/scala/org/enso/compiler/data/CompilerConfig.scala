package org.enso.compiler.data

import java.io.PrintStream

/** Configuration for the compiler.
  *
  * @param autoParallelismEnabled whether or not automatic parallelism detection
  *                               is enabled.
  * @param warningsEnabled whether or not warnings are enabled
  * @param privateCheckEnabled whether or not private keyword is enabled
  * @param staticTypeInferenceEnabled whether or not type inference is enabled
  * @param dumpIrs whether or not to dump IRs. See [[org.enso.compiler.dump.IRDumper]].
  * @param isStrictErrors if true, presence of any Error in IR will result in an exception
  * @oaram isLintingDisabled if true, compilation should not run any linting passes
  * @param outputRedirect redirection of the output of warnings and errors of compiler
  */
case class CompilerConfig(
  autoParallelismEnabled: Boolean     = false,
  warningsEnabled: Boolean            = true,
  privateCheckEnabled: Boolean        = true,
  staticTypeInferenceEnabled: Boolean = false,
  dumpIrs: Boolean                    = false,
  isStrictErrors: Boolean             = false,
  isLintingDisabled: Boolean          = false,
  outputRedirect: Option[PrintStream] = None
) {
  def parallelParsing: Boolean = false
}
