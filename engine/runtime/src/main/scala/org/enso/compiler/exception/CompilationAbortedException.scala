package org.enso.compiler.exception

import com.oracle.truffle.api.exception.AbstractTruffleException

/** An exception thrown to break out of the compilation flow after reporting
  * all the encountered errors.
  */
class CompilationAbortedException extends AbstractTruffleException {
  override def getMessage: String = "Compilation aborted due to errors."
}
