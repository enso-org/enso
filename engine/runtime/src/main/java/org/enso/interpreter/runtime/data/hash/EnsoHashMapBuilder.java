package org.enso.interpreter.runtime.data.hash;

import java.util.Arrays;
import java.util.Iterator;
import java.util.stream.Stream;

import org.enso.interpreter.node.expression.builtin.meta.EqualsNode;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeNode;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;

/**
 * A storage for a {@link EnsoHashMap}. For one builder, there may be many
 * {@link EnsoHashMap} instances that serve as a snapshot.
 *
 * There should be where most one snapshot for a given generation. All the snapshots should
 have generation smaller than this builder generation.
 */
final class EnsoHashMapBuilder implements Iterable<EnsoHashMapBuilder.StorageEntry> {
  /**
  * Array of entries. It is only being added into. Both {@code put} and {@code remove}
  * operations just add new entries into it using <em>linear hashing</em>.
  */
  private final StorageEntry[] byHash;
  /** number of entries in the {@coede byHash} array. With every change to the builder,
  * the generation increases by one. Once the generation reaches 75% of {@code byHash.length}
  * it is time to <em>rehash</em> into new builder.
  */
  private int generation;
  /** the actual number of entries in the builder at the latest {@code generation}.
   * <ul>
   *   <li>{@code put} of new key increases it</li>
   *   <li>{@code put} over existing key doesn't change it</li>
   *   <li>{@code remove} of a key decreases it</li>
   * </ul>
  */
  private int actualSize;

  /** Creates an empty builder with given capacity. The capacity specifies
   * the size of array of {@link StorageEntry} instances. The {@code put}
   * and {@code remove} operations add entries into the array until it is
   * 75% full.
   */
  private EnsoHashMapBuilder(int initialCapacity) {
    this.byHash = new StorageEntry[initialCapacity];
  }

  private EnsoHashMapBuilder(EnsoHashMapBuilder other) {
    this.byHash = other.byHash.clone();
    this.generation = other.generation;
  }

  /**
   * Create a new builder with default size being {@code 11}.
   */
  public static EnsoHashMapBuilder create() {
    return new EnsoHashMapBuilder(11);
  }

  /** Returns count of elements in the storage. */
  public int generation() {
    return generation;
  }

  /** Returns the actual number of visible elements in current generation. */
  public int size() {
    return actualSize;
  }

  /** Provides access to all {@code StorageEntry} in this builder.
   * Classical usage is to {@code for (var e : this) if (e.isVisible(atGeneration) operation(e))}.
   */
  @Override
  public Iterator<StorageEntry> iterator() {
    return Stream.of(byHash).filter((e) -> e != null).iterator();
  }

  /**
   * Prepares a builder ready for modification at given generation.
   * It may return {@code this} if the {@code atGeneration == this.generation}
   * and the {@code byHash} array is less than 75% full. Otherwise
   * it may return new builder suitable for additions.
   */
  public EnsoHashMapBuilder asModifiable(int atGeneration, HashCodeNode hashCodeNode, EqualsNode equalsNode) {
    if (atGeneration != generation || generation * 4 > byHash.length * 3) {
      var newSize = Math.max(actualSize * 2, byHash.length);
      return rehash(newSize, atGeneration, hashCodeNode, equalsNode);
    } else {
      return this;
    }
  }

  /** Adds a key-value mapping. Uses {@code hashCodeNode} to compute
   * hash and based on it location in the array. Then it searches for
   * first empty slot after the identified location. If it finds an
   * equal key, it marks it as removed, if it hasn't been removed yet.
   * Once it finds an empty slot, it puts there a new entry with
   * the next generation.
   */
  public void put(
    Object key, Object value,
    HashCodeNode hashCodeNode, EqualsNode equalsNode
  ) {
    var at = findWhereToStart(key, hashCodeNode);
    var nextGeneration = ++generation;
    var replacingExistingKey = false;
    for (var i = 0; i < byHash.length; i++) {
      if (byHash[at] == null) {
        if (!replacingExistingKey) {
          actualSize++;
        }
        byHash[at] = new StorageEntry(key, value, nextGeneration);
        return;
      }
      if (equalsNode.execute(byHash[at].key(), key)) {
        var invalidatedEntry = byHash[at].markRemoved(nextGeneration);
        if (invalidatedEntry != byHash[at]) {
          byHash[at] = invalidatedEntry;
          replacingExistingKey = true;
        }
      }
      if (++at == byHash.length) {
        at = 0;
      }
    }
    throw CompilerDirectives.shouldNotReachHere("byHash array is full!");
  }

  /** Finds storage entry for given key or null */
  public StorageEntry get(
    Object key,
    HashCodeNode hashCodeNode, EqualsNode equalsNode
  ) {
    var at = findWhereToStart(key, hashCodeNode);
    for (var i = 0; i < byHash.length; i++) {
      if (byHash[at] == null) {
        return null;
      }
      if (equalsNode.execute(key, byHash[at].key())) {
        return byHash[at];
      }
      if (++at == byHash.length) {
        at = 0;
      }
    }
    throw CompilerDirectives.shouldNotReachHere("byHash array is full!");
  }

  private int findWhereToStart(Object key, HashCodeNode hashCodeNode) {
    var hash = Math.abs(hashCodeNode.execute(key));
    var at = (int) (hash % byHash.length);
    return at;
  }

  /**
   * Removes an entry denoted by the given key. Removal is "non-destrutive" - the
   * "removed" entry stays in the array - only its {@link StorageEntry#removed()}
   * value is set to the next generation.
   *
   * @return true if the removal was successful false otherwise.
   */
  public boolean remove(
    Object key,
    HashCodeNode hashCodeNode, EqualsNode equalsNode
  ) {
    var at = findWhereToStart(key, hashCodeNode);
    var nextGeneration = ++generation;
    for (var i = 0; i < byHash.length; i++) {
      if (byHash[at] == null) {
        return false;
      }
      if (equalsNode.execute(key, byHash[at].key())) {
        var invalidatedEntry = byHash[at].markRemoved(nextGeneration);
        if (invalidatedEntry != byHash[at]) {
          byHash[at] = invalidatedEntry;
          actualSize--;
          return true;
        }
      }
      if (++at == byHash.length) {
        at = 0;
      }
    }
    throw CompilerDirectives.shouldNotReachHere("byHash array is full!");
  }

  /** Builds a new builder with given array size and puts into it all entries
   * that are valid {@code atGeneration}.
   */
  private EnsoHashMapBuilder rehash(int size, int atGeneration, HashCodeNode hashCodeNode, EqualsNode equalsNode) {
    var newBuilder = new EnsoHashMapBuilder(size);
    for (var entry : this) {
      if (entry.isVisible(atGeneration)) {
        newBuilder.put(entry.key(), entry.value(), hashCodeNode, equalsNode);
      }
    }
    return newBuilder;
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
    return EnsoHashMap.createWithBuilder(this);
  }

  @Override
  public String toString() {
    return "EnsoHashMapBuilder{size = " + generation + ", storage = " + Arrays.toString(byHash) + "}";
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
