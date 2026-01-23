package org.enso.table.data.column.storage;

import java.util.concurrent.atomic.AtomicLong;
import org.enso.table.data.column.storage.type.StorageType;

/** An abstract representation of a Storage providing a single uniqueKey implementation . */
public abstract class Storage<T> implements ColumnStorage<T> {
  private static final AtomicLong atomicCounter = new AtomicLong(0);

  private final long uniqueKey = atomicCounter.incrementAndGet();
  private final char typeChar;
  private final int typeSize;

  protected Storage(StorageType<T> storageType) {
    this.typeChar = storageType.typeChar();
    this.typeSize = storageType.size();
  }

  @Override
  public final long uniqueKey() {
    return uniqueKey;
  }

  @Override
  public final char typeChar() {
    return typeChar;
  }

  @Override
  public final int typeSize() {
    return typeSize;
  }
}
