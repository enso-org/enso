package org.enso.compiler.core;

public class CompilerError extends RuntimeException {
  public CompilerError(String message) {
    this(message, null);
  }

  public CompilerError(String message, Throwable cause) {
    super("Compiler Internal Error: " + message, cause);
  }
}
