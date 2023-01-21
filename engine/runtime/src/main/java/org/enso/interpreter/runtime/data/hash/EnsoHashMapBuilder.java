package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import java.util.ArrayList;
import java.util.List;
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
  /** All entries stored by their sequential index. */
  private final List<StorageEntry> sequentialEntries;

  private final HashCodeAnyNode hashCodeNode;
  private final EqualsAnyNode equalsNode;
  private int size;

  private EnsoHashMapBuilder(HashCodeAnyNode hashCodeAnyNode, EqualsAnyNode equalsNode) {
    this.storage = EconomicMap.create(new StorageStrategy(equalsNode, hashCodeAnyNode));
    this.sequentialEntries = new ArrayList<>();
    this.hashCodeNode = hashCodeAnyNode;
    this.equalsNode = equalsNode;
  }

  private EnsoHashMapBuilder(EnsoHashMapBuilder other, int numEntries) {
    assert 0 < numEntries && numEntries <= other.size;
    this.storage = EconomicMap.create(new StorageStrategy(other.equalsNode, other.hashCodeNode));
    var entriesToBeDuplicated = other.sequentialEntries.subList(0, numEntries);
    this.sequentialEntries = new ArrayList<>(entriesToBeDuplicated);
    entriesToBeDuplicated.forEach(entry -> this.storage.put(entry.key, entry));
    this.hashCodeNode = other.hashCodeNode;
    this.equalsNode = other.equalsNode;
    this.size = numEntries;
  }

  private EnsoHashMapBuilder(EnsoHashMapBuilder other) {
    this.storage =
        EconomicMap.create(
            new StorageStrategy(other.equalsNode, other.hashCodeNode), other.storage);
    this.sequentialEntries = new ArrayList<>(other.sequentialEntries);
    this.hashCodeNode = other.hashCodeNode;
    this.equalsNode = other.equalsNode;
    this.size = other.size;
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

  /**
   * Duplicates the MapBuilder with just first {@code numEntries} number of entries.
   *
   * @param numEntries Number of entries to take from this MapBuilder.
   */
  public EnsoHashMapBuilder duplicatePartial(int numEntries) {
    return new EnsoHashMapBuilder(this, numEntries);
  }

  /** Duplicates this builder with all its entries. */
  @TruffleBoundary
  public EnsoHashMapBuilder duplicate() {
    return new EnsoHashMapBuilder(this);
  }

  /** Adds a key-value mapping, overriding any existing value. */
  @TruffleBoundary(allowInlining = true)
  public void add(Object key, Object value) {
    var oldEntry = storage.get(key);
    int newEntryIndex = oldEntry != null ? oldEntry.index : size;
    var newEntry = new StorageEntry(key, value, newEntryIndex);
    storage.put(key, newEntry);
    if (oldEntry == null) {
      assert newEntry.index == size;
      sequentialEntries.add(newEntry);
      size++;
    } else {
      sequentialEntries.set(newEntryIndex, newEntry);
    }
  }

  @TruffleBoundary(allowInlining = true)
  public StorageEntry get(Object key) {
    return storage.get(key);
  }

  /**
   * Removes an entry denoted by the given key.
   *
   * @return true if the removal was successful, i.e., the key was in the map and was removed, false
   *     otherwise.
   */
  @TruffleBoundary
  public boolean remove(Object key) {
    var oldEntry = storage.removeKey(key);
    if (oldEntry == null) {
      return false;
    } else {
      sequentialEntries.remove(oldEntry.index);
      // Rewrite rest of the sequentialEntries list and repair indexes in storage
      for (int i = oldEntry.index; i < sequentialEntries.size(); i++) {
        var entry = sequentialEntries.get(i);
        StorageEntry newEntry = new StorageEntry(entry.key, entry.value, i);
        sequentialEntries.set(i, newEntry);
        storage.put(newEntry.key, newEntry);
      }
      size--;
      return true;
    }
  }

  @TruffleBoundary(allowInlining = true)
  public boolean containsKey(Object key) {
    return storage.containsKey(key);
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

  @Override
  public String toString() {
    return "EnsoHashMapBuilder{size = " + size + ", storage = " + storage + "}";
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
