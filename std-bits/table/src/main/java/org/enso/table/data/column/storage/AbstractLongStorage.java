package org.enso.table.data.column.storage;

import org.enso.table.data.column.storage.iterators.ColumnLongStorageIterator;
import org.enso.table.data.column.storage.iterators.LongStorageIterator;
import org.enso.table.data.column.storage.type.IntegerType;

public abstract class AbstractLongStorage extends Storage<Long> implements ColumnLongStorage {
  private final long size;

  protected AbstractLongStorage(long size, IntegerType type) {
    super(type);
    this.size = size;
  }

  @Override
  public IntegerType getType() {
    return (IntegerType) super.getType();
  }

  @Override
  public final long getSize() {
    return size;
  }

  @Override
  public Long getItemBoxed(long index) {
    return isNothing(index) ? null : getItemAsLong(index);
  }

  @Override
  public abstract boolean isNothing(long idx);

  @Override
  public abstract long getItemAsLong(long index) throws ValueIsNothingException;

  /**
   * Return an instance of storage containing the same data but with a wider type.
   *
   * <p>Ideally it should avoid copying the data, if it's possible.
   */
  public abstract AbstractLongStorage widen(IntegerType widerType);

  @Override
  public ColumnLongStorageIterator iteratorWithIndex() {
    return new LongStorageIterator(this);
  }
}
