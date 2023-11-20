package org.enso.persist;

import org.enso.persist.PerInputImpl.InputCache;
import org.enso.persist.Persistance.Reference;

final class PerBufferReference<T> extends Persistance.Reference<T> {
  private final Persistance<T> p;
  private final PerInputImpl.InputCache cache;
  private final int offset;

  private PerBufferReference(Persistance<T> p, PerInputImpl.InputCache buffer, int offset) {
    this.p = p;
    this.cache = buffer;
    this.offset = offset;
  }

  @SuppressWarnings(value = "unchecked")
  final <T> T readObject(Class<T> clazz) {
    if (p != null) {
      if (clazz.isAssignableFrom(p.clazz)) {
        clazz = (Class) p.clazz;
      } else {
        throw new ClassCastException();
      }
    }
    org.enso.persist.PerInputImpl in = new PerInputImpl(cache, offset);
    T obj = in.readInline(clazz);
    return obj;
  }

  static <V> Reference<V> from(InputCache buffer, int offset) {
    return from(null, buffer, offset);
  }

  static <V> Reference<V> from(Persistance<V> p, InputCache buffer, int offset) {
    return new PerBufferReference<>(p, buffer, offset);
  }
}
