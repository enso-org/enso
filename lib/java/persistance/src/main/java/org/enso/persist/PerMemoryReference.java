package org.enso.persist;

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

  static final class Deferred<T> extends PerMemoryReference<T> {
    Deferred(T obj) {
      super(obj);
    }
  }
}
