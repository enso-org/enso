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
    assert Objects.equals(expectedSupplier.get(), actual)
        : msg + "\nexpected: " + expectedSupplier.get() + "\nactual: " + actual;
    return actual;
  }
}
