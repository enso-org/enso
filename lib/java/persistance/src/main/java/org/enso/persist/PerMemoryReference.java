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
  public int hashCode() {
    int hash = 5;
    hash = 31 * hash + Objects.hashCode(this.value);
    return hash;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final PerMemoryReference<?> other = (PerMemoryReference<?>) obj;
    return Objects.equals(this.value, other.value);
  }

  static final class Deferred<T> extends PerMemoryReference<T> {
    Deferred(T obj) {
      super(obj);
    }
  }
}
