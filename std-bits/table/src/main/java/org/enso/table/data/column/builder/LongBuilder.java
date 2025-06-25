package org.enso.table.data.column.builder;

import java.util.Objects;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.ValueTypeMismatchException;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.BitSets;

/** A builder for integer columns. */
public class LongBuilder extends NumericBuilder implements BuilderForLong, BuilderWithRetyping {
  protected final ProblemAggregator problemAggregator;
  protected long[] data;

  protected LongBuilder(int initialSize, ProblemAggregator problemAggregator) {
    this.data = new long[initialSize];
    this.problemAggregator = problemAggregator;
  }

  static LongBuilder make(int initialSize, IntegerType type, ProblemAggregator problemAggregator) {
    if (type == null || type.equals(IntegerType.INT_64)) {
      return new LongBuilder(initialSize, problemAggregator);
    } else {
      return new BoundCheckedIntegerBuilder(initialSize, type, problemAggregator);
    }
  }

  @Override
  protected int getDataSize() {
    return data.length;
  }

  @Override
  protected void resize(int desiredCapacity) {
    long[] newData = new long[desiredCapacity];
    int toCopy = Math.min(currentSize, data.length);
    System.arraycopy(data, 0, newData, 0, toCopy);
    data = newData;
  }

  @Override
  public void copyDataTo(Object[] items) {
    for (int i = 0; i < currentSize; i++) {
      if (isNothing.get(i)) {
        items[i] = null;
      } else {
        items[i] = data[i];
      }
    }
  }

  @Override
  public boolean canRetypeTo(StorageType<?> type) {
    return Objects.equals(type, FloatType.FLOAT_64)
        || Objects.equals(type, BigIntegerType.INSTANCE)
        || Objects.equals(type, BigDecimalType.INSTANCE);
  }

  @Override
  public Builder retypeTo(StorageType<?> type) {
    if (Objects.equals(type, BigIntegerType.INSTANCE)) {
      return BigIntegerBuilder.retypeFromLongBuilder(this);
    } else if (Objects.equals(type, FloatType.FLOAT_64)) {
      return InferredDoubleBuilder.retypeFromLongBuilder(this);
    } else if (Objects.equals(type, BigDecimalType.INSTANCE)) {
      return BigDecimalBuilder.retypeFromLongBuilder(this);
    } else {
      throw new UnsupportedOperationException();
    }
  }

  @Override
  public IntegerType getType() {
    return IntegerType.INT_64;
  }

  @Override
  public boolean accepts(Object o) {
    return NumericConverter.isCoercibleToLong(o);
  }

  @Override
  public void appendBulkStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof IntegerType otherType) {
      if (getType().fits(otherType)) {
        if (storage instanceof LongStorage longStorage) {
          // A fast path for the same type (or compatible) - no conversions/checks needed.
          int n = (int) longStorage.getSize();
          ensureFreeSpaceFor(n);
          System.arraycopy(longStorage.getData(), 0, data, currentSize, n);
          BitSets.copy(longStorage.getIsNothingMap(), isNothing, currentSize, n);
          currentSize += n;
        } else {
          // No conversions needed, but we need to iterate over the items.
          var longStorage = otherType.asTypedStorage(storage);
          long n = longStorage.getSize();
          for (long i = 0; i < n; i++) {
            if (longStorage.isNothing(i)) {
              appendNulls(1);
            } else {
              appendLong(longStorage.getItemAsLong(i));
            }
          }
        }
      }
    } else if (storage instanceof ColumnBooleanStorage boolStorage) {
      long n = boolStorage.getSize();
      for (long i = 0; i < n; i++) {
        if (boolStorage.isNothing(i)) {
          appendNulls(1);
        } else {
          appendLong(boolStorage.getItemAsBoolean(i) ? 1L : 0L);
        }
      }
    } else if (storage.getType() instanceof NullType) {
      appendNulls(Math.toIntExact(storage.getSize()));
    } else {
      throw new StorageTypeMismatchException(getType(), storage.getType());
    }
  }

  /**
   * Append a new integer to this builder.
   *
   * @param value the integer to append
   */
  public LongBuilder appendLong(long value) {
    ensureSpaceToAppend();
    this.data[currentSize++] = value;
    return this;
  }

  @Override
  public LongBuilder appendNulls(int count) {
    doAppendNulls(count);
    return this;
  }

  @Override
  public LongBuilder append(Object o) {
    if (o == null) {
      return appendNulls(1);
    }

    Long x = NumericConverter.tryConvertingToLong(o);
    if (x != null) {
      appendLong(x);
    } else {
      throw new ValueTypeMismatchException(getType(), o);
    }

    return this;
  }

  @Override
  public ColumnStorage<Long> seal() {
    return new LongStorage(data, currentSize, isNothing, getType());
  }
}
