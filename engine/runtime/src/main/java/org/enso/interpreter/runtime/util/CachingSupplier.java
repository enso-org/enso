package org.enso.interpreter.runtime.util;

import java.util.function.Supplier;

public final class CachingSupplier<T> implements Supplier<T> {
  private final Supplier<T> supply;
  private T memo;

  public CachingSupplier(Supplier<T> supply) {
    this.supply = supply;
  }

  public CachingSupplier(T memo) {
    this.supply = null;
    this.memo = memo;
  }

  @Override
  public T get() {
    if (memo == null) {
      memo = supply.get();
    }
    return memo;
  }
}
