package org.enso.compiler.core.ir

import org.enso.compiler.core.IR

trait LazyDiagnosticStorage { self: IR =>

  private[this] var _diagnostics: DiagnosticStorage = _

  def diagnostics: DiagnosticStorage = {
    _diagnostics
  }

  def diagnostics_=(diagnostics: DiagnosticStorage): Unit = {
    assert(_diagnostics eq null)
    _diagnostics = diagnostics
  }

  def diagnosticsList: List[Diagnostic] = {
    if (_diagnostics eq null) Nil else _diagnostics.toList
  }

  def diagnosticsCopy: DiagnosticStorage = {
    if (_diagnostics eq null) _diagnostics else _diagnostics.copy
  }

  def getDiagnostics: DiagnosticStorage = {
    if (_diagnostics eq null) {
      _diagnostics = DiagnosticStorage()
    }
    _diagnostics
  }
}
