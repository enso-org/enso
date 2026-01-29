package org.enso.table.data.column.storage;

import java.util.concurrent.atomic.AtomicLong;
import org.enso.table.data.column.storage.type.StorageType;

/** An abstract representation of a Storage providing a single uniqueKey implementation . */
abstract class AbstractBaseStorage<T> implements ColumnStorage<T> {
  static final AtomicLong atomicCounter = new AtomicLong(0);

  private final long uniqueKey = ColumnStorage.getNextUniqueKey();
  private final char typeChar;
  private final long typeSize;

  protected AbstractBaseStorage(StorageType<T> storageType) {
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
  public final long typeSize() {
    return typeSize;
  }
}
