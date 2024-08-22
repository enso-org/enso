package org.enso.compiler.core.ir

import org.enso.compiler.core.IR

trait LazyDiagnosticStorage { self: IR =>

  private[this] var _diagnostics: DiagnosticStorage = _

  protected def diagnostics_=(diagnostics: DiagnosticStorage): Unit = {
    assert(_diagnostics eq null)
    _diagnostics = diagnostics
  }

  def diagnostics: DiagnosticStorage = {
    _diagnostics
  }

  override def diagnosticsList: List[Diagnostic] = {
    if (_diagnostics eq null) Nil else _diagnostics.toList
  }

  override def diagnosticsCopy: DiagnosticStorage = {
    if (_diagnostics eq null) _diagnostics else _diagnostics.copy
  }

  override def getDiagnostics: DiagnosticStorage = {
    if (_diagnostics eq null) {
      _diagnostics = new DiagnosticStorage()
    }
    _diagnostics
  }
}
