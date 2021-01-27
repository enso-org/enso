package org.enso.interpreter.instrument;

public class MySentinelException extends RuntimeException {

  private final MySentinel sentinel;

  public MySentinelException(MySentinel sentinel) {
    this.sentinel = sentinel;
  }

  public MySentinel getSentinel() {
    return sentinel;
  }
}
