package org.enso.interpreter.runtime.data.hash;

import org.enso.interpreter.node.expression.builtin.meta.EqualsAnyNode;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeAnyNode;
import org.graalvm.collections.EconomicMap;
import org.graalvm.collections.Equivalence;

/**
 * There should be at most one snapshot for a given size.
 * All the snapshots should have size smaller than this builder size.
 */
public final class EnsoHashMapBuilder {
  private EconomicMap<Object, StorageEntry> storage;
  private int size;

  EnsoHashMapBuilder(HashCodeAnyNode hashCodeAnyNode, EqualsAnyNode equalsNode) {
    this.storage = EconomicMap.create(new StorageStrategy(equalsNode, hashCodeAnyNode));
  }

  public static EnsoHashMapBuilder create(HashCodeAnyNode hashCodeNode, EqualsAnyNode equalsNode) {
    return new EnsoHashMapBuilder(hashCodeNode, equalsNode);
  }

  public int getSize() {
    return size;
  }

  public EconomicMap<Object, StorageEntry> getStorage() {
    return storage;
  }

  public void add(Object key, Object value) {
    StorageEntry oldEntry = storage.get(key);
    StorageEntry newEntry =
        oldEntry != null ? new StorageEntry(key, value, oldEntry.index) : new StorageEntry(key, value, size);
    storage.put(key, newEntry);
    if (oldEntry == null) {
      size++;
    }
  }

  public StorageEntry get(Object key) {
    return storage.get(key);
  }

  public EnsoHashMap build() {
    return EnsoHashMap.createWithBuilder(this, size);
  }

  record StorageEntry(
      Object key,
      Object value,
      int index
  ){}

  private static final class StorageStrategy extends Equivalence {
    private final EqualsAnyNode equalsNode;
    private final HashCodeAnyNode hashCodeNode;

    private StorageStrategy(EqualsAnyNode equalsNode, HashCodeAnyNode hashCodeNode) {
      this.equalsNode = equalsNode;
      this.hashCodeNode = hashCodeNode;
    }

    @Override
    public boolean equals(Object a, Object b) {
      return equalsNode.execute(a, b);
    }

    @Override
    public int hashCode(Object o) {
      return (int) hashCodeNode.execute(o);
    }
  }

}
