package org.enso.table.data.column.storage.numeric;

import java.util.BitSet;
import java.util.NoSuchElementException;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnStorageWithNothingMap;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.ValueIsNothingException;
import org.enso.table.data.column.storage.iterators.ColumnDoubleStorageIterator;
import org.enso.table.data.column.storage.iterators.DoubleStorageIterator;
import org.enso.table.data.column.storage.type.FloatType;

/** A column containing floating point numbers. */
public final class DoubleStorage extends Storage<Double>
    implements ColumnDoubleStorage, ColumnStorageWithNothingMap {
  final double[] data;
  final BitSet isNothing;
  private final int size;

  /**
   * @param data the underlying data
   * @param size the number of items stored
   * @param isNothing a bit set denoting at index {@code i} whether the value at index {@code i} is
   *     Nothing.
   */
  public DoubleStorage(double[] data, int size, BitSet isNothing) {
    this.data = data;
    this.isNothing = isNothing;
    this.size = size;
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
  public BitSet getIsNothingMap() {
    return isNothing;
  }

  @Override
  public double getItemAsDouble(long index) throws ValueIsNothingException {
    if (isNothing(index)) {
      throw new ValueIsNothingException(index);
    }
    return data[Math.toIntExact(index)];
  }

  @Override
  public FloatType getType() {
    return FloatType.FLOAT_64;
  }

  @Override
  public boolean isNothing(long idx) {
    if (idx < 0 || idx >= getSize()) {
      throw new IndexOutOfBoundsException(idx);
    }
    return isNothing.get((int) idx);
  }

  /** Allow access to the underlying data array for copying. */
  public double[] getData() {
    return data;
  }

  @Override
  public ColumnDoubleStorageIterator iteratorWithIndex() {
    return new DoubleStorageIterator(data, isNothing, (int) getSize());
  }

  private static class DoubleStorageIterator implements ColumnDoubleStorageIterator {
    private final double[] data;
    private final BitSet isNothing;
    private final int size;
    private int index = -1;

    public DoubleStorageIterator(double[] data, BitSet isNothing, int size) {
      this.data = data;
      this.isNothing = isNothing;
      this.size = size;
    }

    @Override
    public Double getItemBoxed() {
      return isNothing.get(index) ? null : data[index];
    }

    @Override
    public double getItemAsDouble() {
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
