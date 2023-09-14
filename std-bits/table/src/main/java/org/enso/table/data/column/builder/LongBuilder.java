package org.enso.table.data.column.builder;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.cast.ToIntegerStorageConverter;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.util.BitSets;

import java.util.BitSet;
import java.util.Objects;

/**
 * A builder for integer columns.
 */
public abstract class LongBuilder extends NumericBuilder {
  protected LongBuilder(BitSet isMissing, long[] data, int currentSize) {
    super(isMissing, data, currentSize);
  }

  static LongBuilder make(int initialSize, IntegerType type) {
    BitSet isMissing = new BitSet();
    long[] data = new long[initialSize];
    if (type.equals(IntegerType.INT_64)) {
      return new LongBuilderUnchecked(isMissing, data, 0);
    } else {
      return new LongBuilderChecked(isMissing, data, 0, type);
    }
  }

  @Override
  public void writeTo(Object[] items) {
    for (int i = 0; i < currentSize; i++) {
      if (isMissing.get(i)) {
        items[i] = null;
      } else {
        items[i] = data[i];
      }
    }
  }

  @Override
  public boolean canRetypeTo(StorageType type) {
    return Objects.equals(type, FloatType.FLOAT_64) || Objects.equals(type, BigIntegerType.INSTANCE);
  }

  @Override
  public TypedBuilder retypeTo(StorageType type) {
    if (Objects.equals(type, BigIntegerType.INSTANCE)) {
      return BigIntegerBuilder.retypeFromLongBuilder(this);
    } else if (Objects.equals(type, FloatType.FLOAT_64)) {
      return DoubleBuilder.retypeFromLongBuilder(this);
    } else {
      throw new UnsupportedOperationException();
    }
  }

  @Override
  public abstract IntegerType getType();

  @Override
  public boolean accepts(Object o) {
    return NumericConverter.isCoercibleToLong(o);
  }

  @Override
  public void appendBulkStorage(Storage<?> storage) {
    if (Objects.equals(storage.getType(), getType()) && storage instanceof LongStorage longStorage) {
      // A fast path for the same type - no conversions/checks needed.
      int n = longStorage.size();
      ensureFreeSpaceFor(n);
      System.arraycopy(longStorage.getRawData(), 0, data, currentSize, n);
      BitSets.copy(longStorage.getIsMissing(), isMissing, currentSize, n);
      currentSize += n;
    } else if (storage.getType() instanceof IntegerType otherType && getType().fits(otherType)) {
      if (storage instanceof AbstractLongStorage longStorage) {
        int n = longStorage.size();
        ensureFreeSpaceFor(n);
        for (int i = 0; i < n; i++) {
          if (longStorage.isNa(i)) {
            isMissing.set(currentSize++);
          } else {
            appendLongNoGrow(longStorage.getItem(i));
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
          if (boolStorage.isNa(i)) {
            isMissing.set(currentSize++);
          } else {
            data[currentSize++] = ToIntegerStorageConverter.booleanAsLong(boolStorage.getItem(i));
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
   * @param data the integer to append
   */
  public void appendLong(long data) {
    if (currentSize >= this.data.length) {
      grow();
    }

    assert currentSize < this.data.length;
    appendLongNoGrow(data);
  }

  public abstract void appendLongNoGrow(long data);

  /** Append a new integer to this builder, without checking for overflows.
   * <p>
   * Used if the range has already been checked by the caller.
   */
  public void appendLongUnchecked(long data) {
    appendRawNoGrow(data);
  }

  @Override
  public Storage<Long> seal() {
    return new LongStorage(data, currentSize, isMissing, getType());
  }
}
