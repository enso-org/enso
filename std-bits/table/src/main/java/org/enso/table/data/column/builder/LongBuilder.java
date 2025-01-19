package org.enso.table.data.column.builder;

import java.util.Objects;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
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
    if (type.equals(IntegerType.INT_64)) {
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
  public boolean canRetypeTo(StorageType type) {
    return Objects.equals(type, FloatType.FLOAT_64)
        || Objects.equals(type, BigIntegerType.INSTANCE);
  }

  @Override
  public Builder retypeTo(StorageType type) {
    if (Objects.equals(type, BigIntegerType.INSTANCE)) {
      return BigIntegerBuilder.retypeFromLongBuilder(this);
    } else if (Objects.equals(type, FloatType.FLOAT_64)) {
      return InferredDoubleBuilder.retypeFromLongBuilder(this);
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
  public void appendBulkStorage(Storage<?> storage) {
    if (Objects.equals(storage.getType(), getType())
        && storage instanceof LongStorage longStorage) {
      // A fast path for the same type - no conversions/checks needed.
      int n = longStorage.size();
      ensureFreeSpaceFor(n);
      System.arraycopy(longStorage.getRawData(), 0, data, currentSize, n);
      BitSets.copy(longStorage.getIsNothingMap(), isNothing, currentSize, n);
      currentSize += n;
    } else if (storage.getType() instanceof IntegerType otherType && getType().fits(otherType)) {
      if (storage instanceof AbstractLongStorage longStorage) {
        int n = longStorage.size();
        ensureFreeSpaceFor(n);
        for (int i = 0; i < n; i++) {
          if (longStorage.isNothing(i)) {
            isNothing.set(currentSize++);
          } else {
            appendLong(longStorage.getItem(i));
          }
        }
      } else {
        throw new IllegalStateException(
            "Unexpected storage implementation for type INTEGER: "
                + storage
                + ". This is a bug in the Table library.");
      }
    } else if (Objects.equals(storage.getType(), BooleanType.INSTANCE)) {
      if (storage instanceof BoolStorage boolStorage) {
        int n = boolStorage.size();
        for (int i = 0; i < n; i++) {
          if (boolStorage.isNothing(i)) {
            isNothing.set(currentSize++);
          } else {
            data[currentSize++] = boolStorage.getItem(i) ? 1L : 0L;
          }
        }
      } else {
        throw new IllegalStateException(
            "Unexpected storage implementation for type BOOLEAN: "
                + storage
                + ". This is a bug in the Table library.");
      }
    } else {
      throw new StorageTypeMismatchException(getType(), storage.getType());
    }
  }

  /**
   * Append a new integer to this builder.
   *
   * @param value the integer to append
   */
  public void appendLong(long value) {
    if (currentSize >= this.data.length) {
      grow();
    }

    assert currentSize < this.data.length;
    this.data[currentSize++] = value;
  }

  public void appendNoGrow(Object o) {
    if (o == null) {
      isNothing.set(currentSize++);
    } else {
      Long x = NumericConverter.tryConvertingToLong(o);
      if (x != null) {
        this.data[currentSize++] = x;
      } else {
        throw new ValueTypeMismatchException(getType(), o);
      }
    }
  }

  @Override
  public Storage<Long> seal() {
    return new LongStorage(data, currentSize, isNothing, getType());
  }
}
