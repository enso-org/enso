package org.enso.table.data.column.storage.numeric;

import java.math.BigInteger;
import java.util.BitSet;
import java.util.List;
import java.util.NoSuchElementException;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.CachedPropertyCheck;
import org.enso.table.data.column.operation.RequiresNumberFormatting;
import org.enso.table.data.column.storage.ColumnLongStorageIterator;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithNothingMap;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.SliceRange;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

/** A column storing 64-bit integers. */
public final class LongStorage extends AbstractLongStorage
    implements ColumnStorageWithNothingMap, NumericFormattingStorage {

  // TODO [RW] at some point we will want to add separate storage classes for byte, short and int,
  // for more compact storage and more efficient handling of smaller integers; for now we will be
  // handling this just by checking the bounds
  final long[] data;
  final BitSet isNothing;
  private CachedPropertyCheck<Boolean> isNumericFormatRequired;

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

    isNumericFormatRequired =
        new CachedPropertyCheck<>(() -> RequiresNumberFormatting.compute(this, null), false);
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

  private ColumnStorage<?> fillMissingDouble(double arg, ProblemAggregator problemAggregator) {
    var builder = Builder.getForDouble(FloatType.FLOAT_64, getSize(), problemAggregator);
    Context context = Context.getCurrent();
    for (int i = 0; i < getSize(); i++) {
      if (isNothing(i)) {
        builder.appendDouble(arg);
      } else {
        builder.appendLong(data[i]);
      }

      context.safepoint();
    }

    return builder.seal();
  }

  private ColumnStorage<?> fillMissingLong(long arg, ProblemAggregator problemAggregator) {
    final var builder = Builder.getForLong(IntegerType.INT_64, getSize(), problemAggregator);
    Context context = Context.getCurrent();
    for (int i = 0; i < getSize(); i++) {
      if (isNothing(i)) {
        builder.appendLong(arg);
      } else {
        builder.appendLong(data[i]);
      }

      context.safepoint();
    }

    return builder.seal();
  }

  private ColumnStorage<?> fillMissingBigInteger(
      BigInteger bigInteger, ProblemAggregator problemAggregator) {
    var builder = Builder.getForBigInteger(getSize(), problemAggregator);
    Context context = Context.getCurrent();
    for (int i = 0; i < getSize(); i++) {
      builder.append(isNothing(i) ? bigInteger : BigInteger.valueOf(data[i]));
      context.safepoint();
    }
    return builder.seal();
  }

  @Override
  public ColumnStorage<?> fillMissing(
      Value arg, StorageType<?> commonType, ProblemAggregator problemAggregator) {
    if (arg.isNumber()) {
      if (NumericConverter.isCoercibleToLong(arg.as(Object.class))) {
        return fillMissingLong(arg.asLong(), problemAggregator);
      } else if (NumericConverter.isBigInteger(arg)) {
        return fillMissingBigInteger(arg.asBigInteger(), problemAggregator);
      } else {
        return fillMissingDouble(arg.asDouble(), problemAggregator);
      }
    }

    return super.fillMissing(arg, commonType, problemAggregator);
  }

  @Override
  public ColumnStorage<Long> slice(int offset, int limit) {
    int size = (int) getSize();
    int newSize = Math.min(size - offset, limit);
    long[] newData;

    // Special case if slice is after the actual data
    if (offset >= data.length) {
      newData = new long[0];
    } else {
      // Can only copy as much as there is data
      int newDataSize = Math.min(data.length - offset, newSize);
      newData = new long[newDataSize];
      System.arraycopy(data, offset, newData, 0, newDataSize);
    }

    BitSet currentMask = getIsNothingMap();
    BitSet newMask = currentMask.get(offset, offset + limit);
    return new LongStorage(newData, newSize, newMask, getType());
  }

  @Override
  public ColumnStorage<Long> slice(List<SliceRange> ranges) {
    BitSet currentMask = getIsNothingMap();
    int newSize = SliceRange.totalLength(ranges);
    long[] newData = new long[newSize];
    BitSet newIsNothing = new BitSet(newSize);
    int offset = 0;
    Context context = Context.getCurrent();
    for (SliceRange range : ranges) {
      int length = range.end() - range.start();
      System.arraycopy(data, range.start(), newData, offset, length);
      for (int i = 0; i < length; ++i) {
        newIsNothing.set(offset + i, currentMask.get(range.start() + i));
        context.safepoint();
      }
      offset += length;
    }

    return new LongStorage(newData, newSize, newIsNothing, getType());
  }

  /** Widening to a bigger type can be done without copying the data. */
  @Override
  public LongStorage widen(IntegerType widerType) {
    assert widerType.fits(getType());
    return new LongStorage(data, (int) getSize(), getIsNothingMap(), widerType);
  }

  /** Allow access to the underlying data array for copying. */
  public long[] getArray() {
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

  /**
   * Checks if any numbers are large enough for the column to require formatin in the table viz.
   *
   * @return true/false if formatting is required
   */
  @Override
  public Boolean cachedNumericFormatCheck() throws InterruptedException {
    return isNumericFormatRequired.get();
  }
}
