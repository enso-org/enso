package org.enso.base.polyglot;

import org.graalvm.polyglot.Value;

public class WrappedDataflowError extends RuntimeException {
  private final Value error;

  public WrappedDataflowError(Value error) {
    this.error = error;
  }

  public Value getDataflowError() {
    return error;
  }

  @Override
  public String getMessage() {
    return "A dataflow error has been returned from an Enso callback called from Java: "
        + error.toString();
  }
}
