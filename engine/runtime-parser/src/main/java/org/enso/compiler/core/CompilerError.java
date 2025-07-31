package org.enso.compiler.core;

public class CompilerError extends RuntimeException {
  private final StringBuilder info = new StringBuilder();

  public CompilerError(String message) {
    super();
    info.append(message);
  }

  public CompilerError(String message, Throwable cause) {
    super(null, cause);
    info.append("Compiler Internal Error: ").append(message);
  }

  public final void attachInfo(CharSequence seq) {
    if (!info.isEmpty()) {
      info.append("\n");
    }
    info.append(seq);
  }

  @Override
  public final String getMessage() {
    return info.toString();
  }
}
