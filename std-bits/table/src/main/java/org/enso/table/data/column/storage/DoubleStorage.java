package org.enso.table.data.column.storage;

import java.util.NoSuchElementException;
import org.enso.table.data.column.storage.iterators.ColumnDoubleStorageIterator;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.util.ImmutableBitSet;

/** A column containing floating point numbers. */
public final class DoubleStorage extends Storage<Double>
    implements ColumnDoubleStorage, ColumnStorageWithValidityMap {
  private final double[] data;
  private final ImmutableBitSet validityMap;
  private final int size;

  /**
   * @param data the underlying data
   * @param size the number of items stored
   * @param validityMap a bit set denoting at index {@code i} whether there is a real value at that
   *     index.
   */
  public DoubleStorage(double[] data, int size, ImmutableBitSet validityMap) {
    super(FloatType.FLOAT_64);
    this.data = data;
    this.validityMap = validityMap;
    this.size = size;
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
  public Double getItemBoxed(long idx) {
    return isNothing(idx) ? null : data[Math.toIntExact(idx)];
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
    return data[Math.toIntExact(index)];
  }

  @Override
  public boolean isNothing(long idx) {
    if (idx < 0 || idx >= getSize()) {
      throw new IndexOutOfBoundsException(idx);
    }
    return !validityMap.get((int) idx);
  }

  /** Allow access to the underlying data array for copying. */
  public double[] getData() {
    return data;
  }

  @Override
  public ColumnDoubleStorageIterator iteratorWithIndex() {
    return new DoubleStorageIterator(data, validityMap, (int) getSize());
  }

  private static class DoubleStorageIterator implements ColumnDoubleStorageIterator {
    private final double[] data;
    private final ImmutableBitSet validityMap;
    private final int size;
    private int index = -1;

    public DoubleStorageIterator(double[] data, ImmutableBitSet validityMap, int size) {
      this.data = data;
      this.validityMap = validityMap;
      this.size = size;
    }

    @Override
    public Double getItemBoxed() {
      return !validityMap.get(index) ? null : data[index];
    }

    @Override
    public double getItemAsDouble() {
      return data[index];
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
