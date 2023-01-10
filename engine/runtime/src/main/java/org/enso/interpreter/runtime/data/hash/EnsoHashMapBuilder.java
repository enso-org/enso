package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import org.enso.interpreter.node.expression.builtin.meta.EqualsAnyNode;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeAnyNode;
import org.graalvm.collections.EconomicMap;
import org.graalvm.collections.Equivalence;

/**
 * A storage for a {@link EnsoHashMap}. For one builder, there may be many snapshots ({@link
 * EnsoHashMap}). There should be at most one snapshot for a given size. All the snapshots should
 * have size smaller than this builder size.
 */
public final class EnsoHashMapBuilder {
  private final EconomicMap<Object, StorageEntry> storage;
  private int size;

  EnsoHashMapBuilder(HashCodeAnyNode hashCodeAnyNode, EqualsAnyNode equalsNode) {
    this.storage = EconomicMap.create(new StorageStrategy(equalsNode, hashCodeAnyNode));
  }

  /**
   * Create a new builder with stored nodes.
   *
   * @param hashCodeNode Node that will be stored in the storage for invoking `hash_code` on keys.
   * @param equalsNode Node that will be stored in the storage for invoking `==` on keys.
   */
  public static EnsoHashMapBuilder create(HashCodeAnyNode hashCodeNode, EqualsAnyNode equalsNode) {
    return new EnsoHashMapBuilder(hashCodeNode, equalsNode);
  }

  /** Returns count of elements in the storage. */
  public int getSize() {
    return size;
  }

  public EconomicMap<Object, StorageEntry> getStorage() {
    return storage;
  }

  /** Adds a key-value mapping, overriding any existing value. */
  @TruffleBoundary
  public void add(Object key, Object value) {
    StorageEntry oldEntry = storage.get(key);
    StorageEntry newEntry =
        oldEntry != null
            ? new StorageEntry(key, value, oldEntry.index)
            : new StorageEntry(key, value, size);
    storage.put(key, newEntry);
    if (oldEntry == null) {
      size++;
    }
  }

  @TruffleBoundary
  public StorageEntry get(Object key) {
    return storage.get(key);
  }

  /**
   * Creates a snapshot with the current size. The created snapshot contains all the entries that
   * are in the storage as of this moment, i.e., all the entries with their indexes lesser than
   * {@code size}.
   *
   * <p>Should be called at most once for a particular {@code size}.
   *
   * @return A new hash map snapshot.
   */
  public EnsoHashMap build() {
    return EnsoHashMap.createWithBuilder(this, size);
  }

  record StorageEntry(
      Object key,
      Object value,
      /**
       * A sequential index of the entry within this map. {@link EnsoHashMap} uses it for checking
       * whether a certain key belongs in that map.
       */
      int index) {}

  /**
   * Custom {@link Equivalence} used for the {@link EconomicMap} that delegates {@code equals} to
   * {@link EqualsAnyNode} and {@code hash_code} to {@link HashCodeAnyNode}.
   */
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
