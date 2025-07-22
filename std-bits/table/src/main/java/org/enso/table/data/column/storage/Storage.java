package org.enso.table.data.column.storage;

import java.util.concurrent.atomic.AtomicLong;

/** An abstract representation of a Storage providing a single uniqueKey implementation . */
public abstract class Storage<T> implements ColumnStorage<T> {
  private static final AtomicLong atomicCounter = new AtomicLong(0);

  private final long uniqueKey = atomicCounter.incrementAndGet();

  @Override
  public final long uniqueKey() {
    return uniqueKey;
  }
}
