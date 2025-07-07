package org.enso.table.data.column.storage;

import java.util.BitSet;
import java.util.NoSuchElementException;
import org.enso.table.data.column.storage.iterators.ColumnLongStorageIterator;
import org.enso.table.data.column.storage.type.IntegerType;

/** A column storing 64-bit integers. */
public final class LongStorage extends AbstractLongStorage implements ColumnStorageWithNothingMap {
  // TODO [RW] at some point we will want to add separate storage classes for byte, short and int,
  // for more compact storage and more efficient handling of smaller integers; for now we will be
  // handling this just by checking the bounds
  final long[] data;
  final BitSet isNothing;

  /**
   * @param data the underlying data
   * @param size the number of items stored
   * @param isNothing a bit set denoting at index {@code i} whether or not the value at index {@code
   *     i} is missing.
   * @param type the type specifying the bit-width of integers that are allowed in this storage
   */
  public LongStorage(long[] data, int size, BitSet isNothing, IntegerType type) {
    super(size, type);
    this.data = data;
    this.isNothing = isNothing;
  }

  public LongStorage(long[] data, IntegerType type) {
    this(data, data.length, new BitSet(), type);
  }

  @Override
  public long getItemAsLong(long index) {
    return data[(int) index];
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
    return new LongStorage(data, (int) getSize(), getIsNothingMap(), widerType);
  }

  /** Allow access to the underlying data array for copying. */
  public long[] getData() {
    return data;
  }

  @Override
  public ColumnLongStorageIterator iteratorWithIndex() {
    return new LongStorageIterator(data, isNothing, (int) getSize());
  }

  private static class LongStorageIterator implements ColumnLongStorageIterator {
    private final long[] data;
    private final BitSet isNothing;
    private final int size;
    private int index = -1;

    public LongStorageIterator(long[] data, BitSet isNothing, int size) {
      this.data = data;
      this.isNothing = isNothing;
      this.size = size;
    }

    @Override
    public Long getItemBoxed() {
      return isNothing.get(index) ? null : data[index];
    }

    @Override
    public long getItemAsLong() {
      return data[index];
    }

    @Override
    public boolean isNothing() {
      return isNothing.get(index);
    }

    @Override
    public boolean hasNext() {
      return index + 1 < size;
    }

    @Override
    public Long next() {
      if (!hasNext()) {
        throw new NoSuchElementException();
      }
      index++;
      return getItemBoxed();
    }

    @Override
    public long getIndex() {
      return index;
    }

    @Override
    public boolean moveNext() {
      if (!hasNext()) {
        return false;
      }
      index++;
      return true;
    }
  }
}
