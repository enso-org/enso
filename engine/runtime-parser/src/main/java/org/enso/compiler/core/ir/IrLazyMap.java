package org.enso.compiler.core.ir;

import java.io.IOException;
import java.util.AbstractMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import org.enso.persist.Persistance;
import org.enso.persist.Persistance.Reference;

final class IrLazyMap<K, V> extends AbstractMap<K, V> {
  private final Map<K, Entry<K, V>> delegate;

  @SuppressWarnings("unchecked")
  IrLazyMap(Persistance.Input in) throws IOException {
    var map = new LinkedHashMap<K, Entry<K, V>>();
    var n = in.readInt();
    for (var i = 0; i < n; i++) {
      var key = (K) in.readObject();
      var ref = (Reference<V>) in.readReference(Object.class);
      var en = new En<>(key, ref);
      map.put(key, en);
    }
    this.delegate = map;
  }

  @Override
  public Set<Entry<K, V>> entrySet() {
    return new LinkedHashSet<>(this.delegate.values());
  }

  @Override
  public V get(Object key) {
    var entry = this.delegate.get(key);
    return entry == null ? null : entry.getValue();
  }

  private static final class En<K, V> implements Entry<K, V> {
    private final K key;
    private final Reference<V> ref;

    En(K key, Reference<V> ref) {
      this.key = key;
      this.ref = ref;
    }

    @Override
    public K getKey() {
      return key;
    }

    @Override
    @SuppressWarnings("unchecked")
    public V getValue() {
      return (V) ref.get(Object.class);
    }

    @Override
    public V setValue(V value) {
      throw new UnsupportedOperationException();
    }

    @Override
    public int hashCode() {
      int hash = 7;
      hash = 29 * hash + Objects.hashCode(this.key);
      return hash;
    }

    @Override
    public boolean equals(Object obj) {
      if (this == obj) {
        return true;
      }
      if (obj instanceof En<?, ?> other) {
        return Objects.equals(this.key, other.key);
      }
      return false;
    }
  }
}
