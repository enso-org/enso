package org.enso.table.data.column.storage;

import java.nio.LongBuffer;
import java.util.BitSet;
import java.util.NoSuchElementException;
import org.enso.table.data.column.storage.iterators.ColumnLongStorageIterator;
import org.enso.table.data.column.storage.type.IntegerType;

/** A column storing 64-bit integers. */
public final class LongStorage extends AbstractLongStorage implements ColumnStorageWithNothingMap {
  // TODO [RW] at some point we will want to add separate storage classes for byte, short and int,
  // for more compact storage and more efficient handling of smaller integers; for now we will be
  // handling this just by checking the bounds
  private final LongBuffer data;
  final BitSet isNothing;

  /**
   * @param data the underlying data
   * @param isNothing a bit set denoting at index {@code i} whether or not the value at index {@code
   *     i} is missing.
   * @param type the type specifying the bit-width of integers that are allowed in this storage
   */
  public LongStorage(LongBuffer data, BitSet isNothing, IntegerType type) {
    super(data.limit(), type);
    this.data = data;
    this.isNothing = isNothing;
  }

  @Override
  public long getItemAsLong(long index) {
    return data.get((int) index);
  }

  @Override
  public boolean isNothing(long idx) {
    if (idx < 0 || idx >= getSize()) {
      throw new IndexOutOfBoundsException(idx);
    }
    return isNothing.get(Math.toIntExact(idx));
  }

  @Override
  public BitSet getIsNothingMap() {
    return isNothing;
  }

  /** Widening to a bigger type can be done without copying the data. */
  @Override
  public LongStorage widen(IntegerType widerType) {
    assert widerType.fits(getType());
    return new LongStorage(data, getIsNothingMap(), widerType);
  }

  /** Allow access to the underlying data array for copying. */
  public LongBuffer getData() {
    return data.asReadOnlyBuffer();
  }

  @Override
  public ColumnLongStorageIterator iteratorWithIndex() {
    return new LongStorageIterator(data.asReadOnlyBuffer(), isNothing);
  }

  private static class LongStorageIterator implements ColumnLongStorageIterator {
    private final LongBuffer data;
    private final BitSet isNothing;

    public LongStorageIterator(LongBuffer data, BitSet isNothing) {
      this.data = data;
      this.isNothing = isNothing;
    }

    @Override
    public Long getItemBoxed() {
      var index = data.position();
      var item = data.get(index);
      return isNothing.get(index) ? null : item;
    }

    @Override
    public long getItemAsLong() {
      var index = data.position();
      var item = data.get(index);
      return item;
    }

    @Override
    public boolean isNothing() {
      var index = data.position();
      return isNothing.get(index);
    }

    @Override
    public boolean hasNext() {
      var index = data.position();
      return index + 1 < data.limit();
    }

    @Override
    public Long next() {
      if (!hasNext()) {
        throw new NoSuchElementException();
      }
      var index = data.position();
      data.position(index + 1);
      return getItemBoxed();
    }

    @Override
    public long getIndex() {
      return data.position();
    }

    @Override
    public boolean moveNext() {
      if (!hasNext()) {
        return false;
      }
      data.position(data.position() + 1);
      return true;
    }
  }
}
