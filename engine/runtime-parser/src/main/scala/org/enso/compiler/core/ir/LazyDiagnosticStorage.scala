package org.enso.compiler.core.ir

import org.enso.compiler.core.IR

trait LazyDiagnosticStorage { self: IR =>

  private[this] var _diagnostics: DiagnosticStorage = _

  def diagnostics: DiagnosticStorage = {
    _diagnostics
  }

  protected def diagnostics_=(diagnostics: DiagnosticStorage): Unit = {
    assert(_diagnostics eq null)
    _diagnostics = diagnostics
  }

  /** Get all diagnostic info associated with this IR node.
    *
    * @return the list of diagnostics
    */
  def diagnosticsList: List[Diagnostic] = {
    if (_diagnostics eq null) Nil else _diagnostics.toList
  }

  /** Get the copy of diagnostic storage associated with this IR node.
    *
    * @return the copy of diagnostic storage
    */
  def diagnosticsCopy: DiagnosticStorage = {
    if (_diagnostics eq null) _diagnostics else _diagnostics.copy
  }

  /** Get storage for compiler diagnostics associated with this IR node.
    *
    * @return the diagnostic storage of this node
    */
  def getDiagnostics: DiagnosticStorage = {
    if (_diagnostics eq null) {
      _diagnostics = new DiagnosticStorage()
    }
    _diagnostics
  }
}
