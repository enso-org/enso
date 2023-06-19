package org.enso.compiler.exception;

public class CompilerError extends RuntimeException {
  public CompilerError(String message) {
    super(message);
  }
}
