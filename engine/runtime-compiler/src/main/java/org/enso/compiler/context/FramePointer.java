package org.enso.compiler.context;

import java.util.Objects;
import java.util.function.Supplier;

/**
 * A representation of a pointer into a stack frame at a given number of levels above the current.
 */
public record FramePointer(int parentLevel, int frameSlotIdx) {

  public FramePointer {
    assert parentLevel >= 0;
    assert frameSlotIdx >= 0;
  }

  static <V> V assertSame(String msg, V actual, Supplier<V> expectedSupplier) {
    assert checkEquality(actual, expectedSupplier)
        : msg + "\nexpected: " + expectedSupplier.get() + "\nactual: " + actual;
    return actual;
  }

  private static <V> boolean checkEquality(V actual, Supplier<V> expectedSupplier) {
    if (!Objects.equals(expectedSupplier.get(), actual)) {
      return false;
    } else {
      return true;
    }
  }
}
