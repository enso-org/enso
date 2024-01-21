package org.enso.base_test_helpers;

public final class IntHolderEquals {
  public final int value;
  public final Integer boxed;

  public IntHolderEquals(int value) {
    this.value = value;
    this.boxed = value;
  }

  public int hashCode() {
    return value;
  }

  public boolean equals(Object obj) {
    if (obj instanceof IntHolderEquals other) {
      return value == other.value;
    }
    return false;
  }
}
