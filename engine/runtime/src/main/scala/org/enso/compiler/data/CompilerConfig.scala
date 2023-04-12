package org.enso.compiler.data

import java.io.PrintStream

/** Configuration for the compiler.
  *
  * @param autoParallelismEnabled whether or not automatic parallelism detection
  *                               is enabled.
  * @param warningsEnabled whether or not warnings are enabled
  * @param isStrictErrors if true, presence of any Error in IR will result in an exception
  * @param outputRedirect redirection of the output of warnings and errors of compiler
  */
case class CompilerConfig(
  autoParallelismEnabled: Boolean     = false,
  warningsEnabled: Boolean            = true,
  isStrictErrors: Boolean             = false,
  outputRedirect: Option[PrintStream] = None
) {
  def parallelParsing: Boolean = false
}
