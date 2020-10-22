package org.enso.compiler.exception

import com.oracle.truffle.api.TruffleException
import com.oracle.truffle.api.nodes.Node

/** An exception thrown to break out of the compilation flow after reporting
  * all the encountered errors.
  */
class CompilationAbortedException extends Exception with TruffleException {
  override def getMessage: String = "Compilation aborted due to errors."

  override def getLocation: Node = null
}
