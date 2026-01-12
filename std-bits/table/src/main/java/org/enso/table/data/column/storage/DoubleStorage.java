package org.enso.table.data.column.storage;

import java.lang.foreign.MemorySegment;
import java.nio.DoubleBuffer;
import java.util.NoSuchElementException;
import org.enso.table.data.column.storage.iterators.ColumnDoubleStorageIterator;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.util.ImmutableBitSet;

/** A column containing floating point numbers. */
public final class DoubleStorage extends Storage<Double>
    implements ColumnDoubleStorage, ColumnStorageWithValidityMap {
  private final DoubleBuffer data;
  private final ImmutableBitSet validityMap;
  private final int size;

  /** original proxy storage to keep from being garbage collected */
  private final ColumnStorage<?> proxy;

  /**
   * @param data the underlying data
   * @param validityMap a bit set denoting at index {@code i} whether there is a real value at that
   *     index.
   * @param otherStorage reference to proxy storage to prevent it from being GCed while this storage
   *     is used
   */
  public DoubleStorage(
      DoubleBuffer data, ImmutableBitSet validityMap, ColumnStorage<?> otherStorage) {
    super(FloatType.FLOAT_64);
    this.data = data;
    this.validityMap = validityMap;
    this.size = data.limit();
    this.proxy = otherStorage;
  }

  @Override
  public FloatType getType() {
    return (FloatType) super.getType();
  }

  @Override
  public long getSize() {
    return size;
  }

  @Override
  public long addressOfData() {
    return MemorySegment.ofBuffer(data).address();
  }

  @Override
  public long addressOfValidity() {
    return MemorySegment.ofBuffer(validityMap.rawData()).address();
  }

  @Override
  public Double getItemBoxed(long idx) {
    return isNothing(idx) ? null : data.get(Math.toIntExact(idx));
  }

  @Override
  public ImmutableBitSet getValidityMap() {
    return validityMap;
  }

  @Override
  public double getItemAsDouble(long index) throws ValueIsNothingException {
    if (isNothing(index)) {
      throw new ValueIsNothingException(index);
    }
    return data.get(Math.toIntExact(index));
  }

  @Override
  public boolean isNothing(long idx) {
    if (idx < 0 || idx >= getSize()) {
      throw new IndexOutOfBoundsException(idx);
    }
    return !validityMap.get((int) idx);
  }

  /** Allow access to the underlying data array for copying. */
  public DoubleBuffer getData() {
    return data.asReadOnlyBuffer();
  }

  @Override
  public ColumnDoubleStorageIterator iteratorWithIndex() {
    return new DoubleStorageIterator(data, validityMap, (int) getSize());
  }

  private static class DoubleStorageIterator implements ColumnDoubleStorageIterator {
    private final DoubleBuffer data;
    private final ImmutableBitSet validityMap;
    private final int size;
    private int index = -1;

    public DoubleStorageIterator(DoubleBuffer data, ImmutableBitSet validityMap, int size) {
      this.data = data;
      this.validityMap = validityMap;
      this.size = size;
    }

    @Override
    public Double getItemBoxed() {
      return !validityMap.get(index) ? null : data.get(index);
    }

    @Override
    public double getItemAsDouble() {
      return data.get(index);
    }

    @Override
    public boolean isNothing() {
      return !validityMap.get(index);
    }

    @Override
    public boolean hasNext() {
      return index + 1 < size;
    }

    @Override
    public Double next() {
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
