package org.enso.compiler.test.mock;

import org.enso.compiler.core.ir.Diagnostic;

public final class DiagnosticException extends RuntimeException {

  public final MockModule module;
  public final Diagnostic diagnostic;
  final boolean isOutputRedirected;

  DiagnosticException(MockModule module, Diagnostic diagnostic, boolean isOutputRedirected) {
    this.module = module;
    this.diagnostic = diagnostic;
    this.isOutputRedirected = isOutputRedirected;
  }
}
