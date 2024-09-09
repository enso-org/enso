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
  private final Map<K, En<K, V>> delegate;

  @SuppressWarnings("unchecked")
  IrLazyMap(Persistance.Input in) throws IOException {
    var map = new LinkedHashMap<K, En<K, V>>();
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

  @Override
  public boolean equals(Object other) {
    if (other instanceof IrLazyMap otherMap) {
      var myEntries = delegate.values();
      var otherEntries = otherMap.delegate.values();
      if (myEntries.size() != otherEntries.size()) {
        return false;
      }
      var myIt = myEntries.iterator();
      var otherIt = otherEntries.iterator();
      while (myIt.hasNext()) {
        var myElem = myIt.next();
        var otherElem = otherIt.next();
        if (!myElem.equals(otherElem)) {
          return false;
        }
      }
      return true;
    } else {
      return super.equals(other);
    }
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
      int hash = 3;
      hash = 19 * hash + Objects.hashCode(this.key);
      hash = 19 * hash + Objects.hashCode(this.ref);
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
      final En<?, ?> other = (En<?, ?>) obj;
      if (!Objects.equals(this.key, other.key)) {
        return false;
      }
      return Objects.equals(this.ref, other.ref);
    }
  }
}
