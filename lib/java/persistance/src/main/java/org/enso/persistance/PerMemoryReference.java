package org.enso.persistance;

final class PerMemoryReference<T> extends Persistance.Reference<T> {
  static final Persistance.Reference<?> NULL = new PerMemoryReference<>(null);
  private final T value;

  PerMemoryReference(T obj) {
    this.value = obj;
  }

  final T value() {
    return value;
  }
}
