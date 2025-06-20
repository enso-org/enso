package org.enso.table.data.column.storage.numeric;

import java.util.BitSet;
import java.util.List;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithNothingMap;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.ValueIsNothingException;
import org.enso.table.data.column.storage.iterators.ColumnDoubleStorageIterator;
import org.enso.table.data.column.storage.iterators.DoubleStorageIterator;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.enso.table.problems.BlackholeProblemAggregator;
import org.graalvm.polyglot.Context;

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

  @Override
  public ColumnStorage<Double> applyFilter(BitSet filterMask, int newLength) {
    var builder =
        Builder.getForDouble(FloatType.FLOAT_64, newLength, BlackholeProblemAggregator.INSTANCE);
    Context context = Context.getCurrent();
    for (int i = 0; i < size; i++) {
      if (filterMask.get(i)) {
        if (isNothing.get(i)) {
          builder.appendNulls(1);
        } else {
          builder.appendDouble(data[i]);
        }
      }
      context.safepoint();
    }
    return builder.seal();
  }

  @Override
  public ColumnStorage<Double> applyMask(OrderMask mask) {
    double[] newData = new double[mask.length()];
    BitSet newIsNothing = new BitSet();
    Context context = Context.getCurrent();
    for (int i = 0; i < mask.length(); i++) {
      int position = mask.get(i);
      if (position == OrderMask.NOT_FOUND_INDEX || isNothing.get(position)) {
        newIsNothing.set(i);
      } else {
        newData[i] = data[position];
      }

      context.safepoint();
    }
    return new DoubleStorage(newData, newData.length, newIsNothing);
  }

  @Override
  public ColumnStorage<Double> slice(List<SliceRange> ranges) {
    int newSize = SliceRange.totalLength(ranges);
    double[] newData = new double[newSize];
    BitSet newIsNothing = new BitSet(newSize);
    int offset = 0;
    Context context = Context.getCurrent();
    for (SliceRange range : ranges) {
      int length = range.end() - range.start();
      System.arraycopy(data, range.start(), newData, offset, length);
      for (int i = 0; i < length; ++i) {
        newIsNothing.set(offset + i, isNothing.get(range.start() + i));
        context.safepoint();
      }
      offset += length;
    }

    return new DoubleStorage(newData, newSize, newIsNothing);
  }

  /** Allow access to the underlying data array for copying. */
  public double[] getArray() {
    return data;
  }

  @Override
  public ColumnDoubleStorageIterator iteratorWithIndex() {
    return new DoubleStorageIterator(this);
  }
}
