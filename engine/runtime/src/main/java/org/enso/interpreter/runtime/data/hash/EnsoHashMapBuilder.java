package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.TruffleLogger;
import java.util.ArrayList;
import java.util.List;
import org.graalvm.collections.EconomicMap;

/**
 * There should be at most one snapshot for a given size.
 * All the snapshots should have size smaller than this builder size.
 */
public class EnsoHashMapBuilder {
  static final TruffleLogger logger = TruffleLogger.getLogger("enso", "HashMap");
  private final List<EnsoHashMap> snapshots = new ArrayList<>();
  private final EconomicMap<Object, ValueWithIndex> storage = EconomicMap.create();
  private int size;
  // TODO: Remove?
  private final int capacity;

  private EnsoHashMapBuilder(int capacity) {
    this.capacity = capacity;
  }

  public static EnsoHashMapBuilder createWithCapacity(int capacity) {
    return new EnsoHashMapBuilder(capacity);
  }

  public void add(Object key, Object value) {
    size++;
    storage.put(key, new ValueWithIndex(value, size));
    // TODO: If size >= capacity then throw
  }

  public int getSize() {
    return size;
  }

  public EconomicMap<Object, ValueWithIndex> getStorage() {
    return storage;
  }

  public int getCapacity() {
    return capacity;
  }

  public List<EnsoHashMap> getSnapshots() {
    return snapshots;
  }

  public ValueWithIndex get(Object key) {
    return storage.get(key);
  }

  public EnsoHashMap build() {
    var snapshot = EnsoHashMap.createWithBuilder(this, size);
    snapshots.add(snapshot);
    return snapshot;
  }

  record ValueWithIndex (
      Object value,
      int index
  ){}
}
