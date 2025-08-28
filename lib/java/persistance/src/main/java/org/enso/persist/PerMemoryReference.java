package org.enso.persist;

import java.util.Objects;

sealed class PerMemoryReference<T> extends Persistance.Reference<T>
    permits PerMemoryReference.Deferred {
  static final Persistance.Reference<?> NULL = new PerMemoryReference<>(null);
  private final T value;

  PerMemoryReference(T obj) {
    this.value = obj;
  }

  final T value() {
    return value;
  }

  @Override
  boolean isDeferredWrite() {
    return Deferred.class == getClass();
  }

  @Override
  public boolean equals(Object o) {
    if (o instanceof PerMemoryReference<?> that) {
      return Objects.equals(value, that.value);
    } else {
      return false;
    }
  }

  @Override
  public int hashCode() {
    return Objects.hashCode(value);
  }

  @Override
  public String toString() {
    return Objects.toString(value);
  }

  static final class Deferred<T> extends PerMemoryReference<T> {
    Deferred(T obj) {
      super(obj);
    }
  }
}
