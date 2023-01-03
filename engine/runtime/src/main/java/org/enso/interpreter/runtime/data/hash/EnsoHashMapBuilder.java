package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.TruffleLogger;
import java.util.ArrayList;
import java.util.List;
import org.enso.interpreter.node.expression.builtin.meta.EqualsAnyNode;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeAnyNode;
import org.graalvm.collections.EconomicMap;

/**
 * There should be at most one snapshot for a given size.
 * All the snapshots should have size smaller than this builder size.
 */
public class EnsoHashMapBuilder {
  static final TruffleLogger logger = TruffleLogger.getLogger("enso", "HashMap");
  // TODO: List of weak refs?
  private final List<EnsoHashMap> snapshots = new ArrayList<>();
  private final EconomicMap<Integer, StorageEntry> storage = EconomicMap.create();
  private int size;

  public static EnsoHashMapBuilder create() {
    return new EnsoHashMapBuilder();
  }

  public int getSize() {
    return size;
  }

  public EconomicMap<Integer, StorageEntry> getStorage() {
    return storage;
  }

  public List<EnsoHashMap> getSnapshots() {
    return snapshots;
  }

  public void add(Object key, Object value, HashCodeAnyNode hashCodeNode, EqualsAnyNode equalsNode) {
    logger.entering("EnsoHashMapBuilder", "add");
    size++;
    int keyHashCode = (int) hashCodeNode.execute(key);
    logger.fine(() -> "Storage = " + storage);
    logger.info(() -> String.format("Inserting key=%s value=%s. keyHashCode=%d", key, value, keyHashCode));
    var entryWithOffset = findEntryLinearProbe(key, equalsNode, hashCodeNode);
    if (entryWithOffset.entry == null) {
      // entry does not exist
      logger.info(() -> "Entry does not exist");
      storage.put(keyHashCode + entryWithOffset.offset, new StorageEntry(key, value, size));
    } else {
      // entry already exist, rewriting
      logger.info(() -> "Entry exists, rewriting");
      storage.put(keyHashCode + entryWithOffset.offset, new StorageEntry(key, value, size));
    }
    logger.exiting("EnsoHashMapBuilder", "add");
  }

  public StorageEntry get(Object key, HashCodeAnyNode hashCodeNode, EqualsAnyNode equalsNode) {
    return findEntryLinearProbe(key, equalsNode, hashCodeNode).entry;
  }

  public EnsoHashMap build() {
    var snapshot = EnsoHashMap.createWithBuilder(this, size);
    snapshots.add(snapshot);
    return snapshot;
  }

  private EntryWithOffset findEntryLinearProbe(Object key, EqualsAnyNode equalsNode, HashCodeAnyNode hashCodeNode) {
    return findEntryLinearProbe(
        (int) hashCodeNode.execute(key),
        0,
        key,
        equalsNode
    );
  }

  private EntryWithOffset findEntryLinearProbe(int keyHashCode, int hashOffset, Object key, EqualsAnyNode equalsNode) {
    StorageEntry entry = storage.get(keyHashCode + hashOffset);
    if (entry == null) {
      return new EntryWithOffset(null, hashOffset);
    } else {
      if (equalsNode.execute(key, entry.key)) {
        return new EntryWithOffset(entry, hashOffset);
      } else {
        return findEntryLinearProbe(keyHashCode, ++hashOffset, key, equalsNode);
      }
    }
  }

  record StorageEntry(
      Object key,
      Object value,
      int index
  ){}

  private record EntryWithOffset(
      StorageEntry entry,
      int offset
  ) {}
}
