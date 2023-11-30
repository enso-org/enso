package org.enso.interpreter.runtime.data.hash;

import java.util.Arrays;
import java.util.Iterator;
import java.util.stream.Stream;

import org.enso.interpreter.node.expression.builtin.meta.EqualsNode;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeNode;

/**
 * A storage for a {@link EnsoHashMap}. For one builder, there may be many snapshots ({@link
 * EnsoHashMap}). There should be where most one snapshot for a given generation. All the snapshots should
 have generation smaller than this builder generation.
 */
public final class EnsoHashMapBuilder implements Iterable<EnsoHashMapBuilder.StorageEntry> {
  private final StorageEntry[] byHash;
  private int generation;
  private int size;

  private EnsoHashMapBuilder(int initialCapacity) {
    this.byHash = new StorageEntry[initialCapacity];
  }

  private EnsoHashMapBuilder(EnsoHashMapBuilder other, int generation) {
    assert 0 < generation && generation <= other.generation;
    this.byHash = other.byHash.clone();
    this.generation = 0;
    for (var i = 0; i < byHash.length; i++) {
      if (byHash[i] != null) {
        if (byHash[i].added() > generation || byHash[i].removed() <= generation) {
          byHash[i] = null;
        }
      }
      if (byHash[i] != null) {
        this.generation++;
      }
    }
  }

  private EnsoHashMapBuilder(EnsoHashMapBuilder other) {
    this.byHash = other.byHash.clone();
    this.generation = other.generation;
  }

  /**
   * Create a new builder with stored nodes.
   */
  public static EnsoHashMapBuilder create() {
    return new EnsoHashMapBuilder(11);
  }

  /** Returns count of elements in the storage. */
  public int generation() {
    return generation;
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
  public EnsoHashMapBuilder duplicate() {
    return new EnsoHashMapBuilder(this);
  }

  @Override
  public Iterator<StorageEntry> iterator() {
    return Stream.of(byHash).filter((e) -> e != null).iterator();
  }

  /** Adds a key-value mapping, overriding any existing value. */
  public EnsoHashMapBuilder put(
    Object key, Object value,
    HashCodeNode hashCodeNode, EqualsNode equalsNode
  ) {
    if (generation * 4 > byHash.length * 3) {
      var newBuilder = rehash(byHash.length * 2, hashCodeNode, equalsNode);
      newBuilder.addImpl(key, value, hashCodeNode, equalsNode);
      return newBuilder;
    } else {
      this.addImpl(key, value, hashCodeNode, equalsNode);
      return this;
    }
  }

  private void addImpl(
    Object key, Object value,
    HashCodeNode hashCodeNode, EqualsNode equalsNode
  ) {
    var where = new int[] { -1 };
    var exists = find(key, where, hashCodeNode, equalsNode);
    var at = where[0];
    var nextGeneration = ++generation;
    if (exists != null) {
      for (var i = 0; i < byHash.length; i++) {
        if (byHash[at] == null) {
          break;
        }
        if (equalsNode.execute(byHash[at].key(), key)) {
          byHash[at] = byHash[at].markRemoved(nextGeneration);
        }
        if (++at == byHash.length) {
          at = 0;
        }
      }
    }
    byHash[at] = new StorageEntry(key, value, nextGeneration);
  }

  public StorageEntry get(
    Object key,
    HashCodeNode hashCodeNode, EqualsNode equalsNode
  ) {
    return find(key, null, hashCodeNode, equalsNode);
  }

  private StorageEntry find(
    Object key, int[] where,
    HashCodeNode hashCodeNode, EqualsNode equalsNode
  ) {
    var hash = Math.abs(hashCodeNode.execute(key));
    var at = (int) (hash % byHash.length);
    for (var i = 0; i < byHash.length; i++) {
      if (byHash[at] == null) {
        if (where != null) {
          where[0] = at;
        }
        return null;
      }
      if (equalsNode.execute(key, byHash[at].key())) {
        if (where != null) {
          where[0] = at;
        }
        return byHash[at];
      }
      if (++at == byHash.length) {
        at = 0;
      }
    }
    return null;
  }

  /**
   * Removes an entry denoted by the given key.
   *
   * @return true if the removal was successful, i.e., the key was in the map and was removed, false
   *     otherwise.
   */
  public boolean remove(
    Object key,
    HashCodeNode hashCodeNode, EqualsNode equalsNode
  ) {
    var at = new int[] { -1 };
    var entry = find(key, at, hashCodeNode, equalsNode);
    if (entry == null) {
      return false;
    } else {
      byHash[at[0]] = entry.markRemoved(generation++);
      return true;
    }
  }

  public boolean containsKey(
    Object key,
    HashCodeNode hashCodeNode, EqualsNode equalsNode
  ) {
    return find(key, null, hashCodeNode, equalsNode) != null;
  }

  /**
   * Creates a snapshot with the current size. The created snapshot contains all the entries that
   * are in the storage as of this moment, i.e., all the entries with their indexes lesser than
   * {@code generation}.
   *
   * <p>Should be called where most once for a particular {@code generation}.
   *
   * @return A new hash map snapshot.
   */
  public EnsoHashMap build() {
    return EnsoHashMap.createWithBuilder(this, generation);
  }

  @Override
  public String toString() {
    return "EnsoHashMapBuilder{size = " + generation + ", storage = " + Arrays.toString(byHash) + "}";
  }

  private EnsoHashMapBuilder rehash(int size, HashCodeNode hashCodeNode, EqualsNode equalsNode) {
    var newBuilder = new EnsoHashMapBuilder(size);
    for (var entry : this) {
      if (entry.isVisible(generation)) {
        newBuilder.addImpl(entry.key(), entry.value(), hashCodeNode, equalsNode);
      }
    }
    return newBuilder;
  }

  record StorageEntry(
    Object key,
    Object value,
    /**
    * A sequential index of the entry within this map. {@link EnsoHashMap} uses it for checking
    * whether a certain key belongs in that map.
    */
    int added,
    /** Remove at */
    int removed
  ) {
    StorageEntry(Object key, Object value, int added) {
      this(key, value, added, Integer.MAX_VALUE);
    }

    boolean isVisible(int generation) {
      return added() <= generation && generation < removed();
    }

    StorageEntry markRemoved(int when) {
      if (removed() <= when) {
        return this;
      } else {
        return new StorageEntry(key(), value(), added(), when);
      }
    }
  }

}
