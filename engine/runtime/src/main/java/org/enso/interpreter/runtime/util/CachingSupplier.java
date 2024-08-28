package org.enso.interpreter.runtime.util;

import com.oracle.truffle.api.CompilerDirectives;
import java.util.function.Supplier;

public final class CachingSupplier<T> implements Supplier<T> {
  @SuppressWarnings("unchecked")
  private static final Supplier EMPTY = new CachingSupplier(null);

  private final Supplier<T> supply;
  @CompilerDirectives.CompilationFinal private boolean memoComputed;
  @CompilerDirectives.CompilationFinal private T memo;

  public CachingSupplier(Supplier<T> supply) {
    this.supply = supply;
  }

  public CachingSupplier(T memo) {
    this.supply = null;
    this.memo = memo;
  }

  @Override
  public T get() {
    synchronized (this) {
      if (memoComputed) {
        return memo;
      }
      CompilerDirectives.transferToInterpreterAndInvalidate();
      if (supply == null) {
        memoComputed = true;
        return memo;
      }
    }
    var v = supply.get();
    synchronized (this) {
      if (!memoComputed) {
        memo = v;
      }
      return memo;
    }
  }

  /**
   * Returns a supplier that always returns {@code null} when its {@link Supplier#get()} method is
   * called.
   *
   * @return non-{@code null} instance of supplier
   */
  @SuppressWarnings("unchecked")
  public static <V> Supplier<V> nullSupplier() {
    return EMPTY;
  }
}
