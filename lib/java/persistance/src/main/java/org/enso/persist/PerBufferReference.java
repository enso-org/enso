package org.enso.persist;

import java.io.IOException;
import org.enso.persist.PerInputImpl.InputCache;
import org.enso.persist.Persistance.Reference;

final class PerBufferReference<T> extends Persistance.Reference<T> {
  private final Persistance<T> p;
  private final PerInputImpl.InputCache cache;
  private final int offset;
  private Object cached;

  private PerBufferReference(
      Persistance<T> p, PerInputImpl.InputCache buffer, int offset, boolean allowCaching) {
    this.p = p;
    this.cache = buffer;
    this.offset = offset;
    this.cached = allowCaching ? null : this;
  }

  @SuppressWarnings(value = "unchecked")
  final <T> T readObject(Class<T> clazz) throws IOException {
    if (cached != this && clazz.isInstance(cached)) {
      return clazz.cast(cached);
    }
    if (p != null) {
      if (clazz.isAssignableFrom(p.clazz)) {
        clazz = (Class) p.clazz;
      } else {
        throw new ClassCastException();
      }
    }
    org.enso.persist.PerInputImpl in = new PerInputImpl(cache, offset);
    T obj = in.readInline(clazz);
    if (cached != this) {
      cached = obj;
    }
    return obj;
  }

  static <V> Reference<V> from(InputCache buffer, int offset) {
    return from(null, buffer, offset);
  }

  static <V> Reference<V> from(Persistance<V> p, InputCache buffer, int offset) {
    return new PerBufferReference<>(p, buffer, offset, false);
  }

  static <V> Reference<V> cached(Persistance<V> p, InputCache buffer, int offset) {
    return new PerBufferReference<>(p, buffer, offset, true);
  }
}
