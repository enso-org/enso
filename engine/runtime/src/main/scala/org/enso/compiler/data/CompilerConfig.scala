package org.enso.compiler.data

/** Configuration for the compiler.
  *
  * @param autoParallelismEnabled whether or not automatic parallelism detection
  *                               is enabled.
  * @param warningsEnabled whether or not warnings are enabled
  * @param silent if true, no errors or warnings are reported to the output
  */
case class CompilerConfig(
  autoParallelismEnabled: Boolean = false,
  warningsEnabled: Boolean        = true,
  silent: Boolean                 = false
)
