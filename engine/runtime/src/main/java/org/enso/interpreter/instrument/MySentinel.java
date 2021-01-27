package org.enso.interpreter.instrument;

import java.util.Arrays;
import java.util.UUID;

public class MySentinel {

  private final UUID[] trace;

  public MySentinel(UUID[] trace) {
    this.trace = trace;
  }

  public MySentinel(UUID expressionId) {
    this.trace = new UUID[] { expressionId };
  }

  public MySentinel() {
    this.trace = new UUID[0];
  }

  public UUID[] getTrace() {
    return trace;
  }

  public MySentinel withTrace(UUID id) {
    UUID [] newTrace = Arrays.copyOf(trace, trace.length + 1);
    newTrace[trace.length - 1] = id;
    return new MySentinel(newTrace);
  }
}
