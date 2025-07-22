package org.enso.compiler.test.mock;

import org.enso.compiler.core.ir.Diagnostic;

public final class DiagnosticException extends RuntimeException {

  public final MockModule module;
  public final Diagnostic diagnostic;
  private final boolean isOutputRedirected;

  DiagnosticException(MockModule module, Diagnostic diagnostic, boolean isOutputRedirected) {
    super(createMessage(module, diagnostic));
    this.module = module;
    this.diagnostic = diagnostic;
    this.isOutputRedirected = isOutputRedirected;
  }

  private static String createMessage(MockModule module, Diagnostic diagnostic) {
    String sb =
        "Failure in module '"
            + module.getName()
            + "' : "
            + diagnostic.message(module::getSourceSection);
    return sb;
  }
}
