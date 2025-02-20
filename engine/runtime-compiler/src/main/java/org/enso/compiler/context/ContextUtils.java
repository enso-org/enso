package org.enso.compiler.context;

import java.util.Objects;
import java.util.function.Supplier;

final class ContextUtils {
  private ContextUtils() {}

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
