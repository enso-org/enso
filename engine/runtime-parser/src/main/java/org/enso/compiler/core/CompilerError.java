package org.enso.compiler.core;

public class CompilerError extends RuntimeException {
  public CompilerError(String message) {
    super("Compiler Internal Error: " + message);
  }
}
