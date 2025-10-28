package org.enso.table.data.column.storage;

import java.util.concurrent.atomic.AtomicLong;
import org.enso.table.data.column.storage.type.StorageType;

/** An abstract representation of a Storage providing a single uniqueKey implementation . */
public abstract class Storage<T> implements ColumnStorage<T> {
  private static final AtomicLong atomicCounter = new AtomicLong(0);

  private final long uniqueKey = atomicCounter.incrementAndGet();
  private final char typeChar;
  private final long size;

  protected Storage(StorageType<T> storageType) {
    this.typeChar = storageType.typeChar();
    this.size = storageType.size();
  }

  @Override
  public final long uniqueKey() {
    return uniqueKey;
  }

  @Override
  public StorageType<T> getType() {
    // The cast is safe because the typeChar and size are taken from the storageType.
    @SuppressWarnings("unchecked")
    var output = (StorageType<T>) StorageType.fromTypeCharAndSize(typeChar, size);
    return output;
  }
}
