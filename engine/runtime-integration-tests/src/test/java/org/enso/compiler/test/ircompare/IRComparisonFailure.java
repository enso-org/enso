package org.enso.compiler.test.ircompare;

import org.enso.compiler.core.IR;

public final class IRComparisonFailure extends AssertionError {
  private final IR expected;
  private final IR actual;

  public IRComparisonFailure(String message, IR expected, IR actual) {
    super(message);
    this.expected = expected;
    this.actual = actual;
  }

  public IR getExpected() {
    return expected;
  }

  public IR getActual() {
    return actual;
  }
}
