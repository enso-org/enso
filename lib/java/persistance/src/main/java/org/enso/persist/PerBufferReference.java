package org.enso.persist;

import java.io.IOException;
import java.util.Objects;
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
    assert p != null;
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

    if (clazz.isAssignableFrom(p.clazz)) {
      clazz = (Class<T>) p.clazz;
    } else {
      throw new ClassCastException(
          "Expecting " + clazz.getName() + " but found " + p.clazz.getName());
    }

    var in = new PerInputImpl(cache, offset);
    T obj = in.readInline(clazz);
    if (cached != this) {
      cached = obj;
    }
    return obj;
  }

  @Override
  boolean isDeferredWrite() {
    return true;
  }

  static <V> Reference<V> from(Persistance<V> p, InputCache buffer, int offset) {
    return new PerBufferReference<>(p, buffer, offset, false);
  }

  static <V> Reference<V> cached(Persistance<V> p, InputCache buffer, int offset) {
    return new PerBufferReference<>(p, buffer, offset, true);
  }

  @Override
  public int hashCode() {
    int hash = 5;
    hash = 97 * hash + Objects.hashCode(this.p);
    hash = 97 * hash + Objects.hashCode(this.cache);
    hash = 97 * hash + this.offset;
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
    final PerBufferReference<?> other = (PerBufferReference<?>) obj;
    if (this.offset != other.offset) {
      return false;
    }
    if (!Objects.equals(this.p, other.p)) {
      return false;
    }
    return Objects.equals(this.cache, other.cache);
  }
}
