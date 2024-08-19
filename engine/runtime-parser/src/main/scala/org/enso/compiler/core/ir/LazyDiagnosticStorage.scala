package org.enso.compiler.core.ir

import org.enso.compiler.core.IR

trait LazyDiagnosticStorage { self: IR =>

  protected var _diagnostics: DiagnosticStorage = _

  override def diagnostics: DiagnosticStorage = {
    if (_diagnostics eq null) {
      _diagnostics = DiagnosticStorage()
    }
    _diagnostics
  }

  override def diagnosticsList: List[Diagnostic] = {
    if (_diagnostics eq null) Nil else _diagnostics.toList
  }

  def diagnostics_=(diagnostics: DiagnosticStorage): Unit = {
    assert(_diagnostics eq null)
    _diagnostics = diagnostics
  }

}
