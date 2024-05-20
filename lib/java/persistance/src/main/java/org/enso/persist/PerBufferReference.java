package org.enso.persist;

import java.io.IOException;
import org.enso.persist.PerInputImpl.InputCache;
import org.enso.persist.Persistance.Reference;

final class PerBufferReference<T> extends Persistance.Reference<T> {
  private final Persistance<T> p;
  private final PerInputImpl.InputCache cache;
  private final int offset;

  /**
   * References can be cached, or loaded again every time.
   *
   * <p>If {@code cached} is set to {@code this}, then the caching is disabled and {@link
   * #get(Class<V>)} will always load a new instance of the object. This is the mode one gets when
   * using an API method {@link Persistance.Input#readReference(Class<T>)}.
   *
   * <p>In other cases the {@code cached} value can be {@code null} meaning <em>not yet loaded</em>
   * or non-{@code null} holding the cached value to be returned from the {@link #get(Class<V>)}
   * method until this reference instance is GCed.
   */
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
    var in = new PerInputImpl(cache, offset);
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
