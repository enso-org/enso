package org.enso.base_test_helpers;

public final class IntHolder {
  public final int value;
  public final Integer boxed;

  public IntHolder(int value) {
    this.value = value;
    this.boxed = value;
  }
}
