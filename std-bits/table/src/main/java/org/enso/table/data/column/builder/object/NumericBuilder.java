package org.enso.table.data.column.builder.object;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.DoubleStorage;
import org.enso.table.data.column.storage.LongStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.util.BitSets;

import java.util.Arrays;
import java.util.BitSet;

/** A builder for numeric columns. */
public class NumericBuilder extends TypedBuilder {
  private final BitSet isMissing = new BitSet();
  private long[] data;
  private boolean isDouble;
  private int currentSize;

  private NumericBuilder(boolean isDouble, int size) {
    this.data = new long[size];
    this.isDouble = isDouble;
  }

  public static NumericBuilder createDoubleBuilder(int size) {
    return new NumericBuilder(true, size);
  }

  public static NumericBuilder createLongBuilder(int size) {
    return new NumericBuilder(false, size);
  }

  @Override
  public void writeTo(Object[] items) {
    for (int i = 0; i < currentSize; i++) {
      if (isMissing.get(i)) {
        items[i] = null;
      } else if (isDouble) {
        items[i] = Double.longBitsToDouble(data[i]);
      } else {
        items[i] = data[i];
      }
    }
  }

  @Override
  public boolean canRetypeTo(long type) {
    return !this.isDouble && type == Storage.Type.DOUBLE;
  }

  @Override
  public TypedBuilder retypeTo(long type) {
    if (!this.isDouble && type == Storage.Type.DOUBLE) {
      this.isDouble = true;
      for (int i = 0; i < currentSize; i++) {
        data[i] = Double.doubleToRawLongBits(data[i]);
      }
      return this;
    } else {
      throw new UnsupportedOperationException();
    }
  }

  @Override
  public int getType() {
    return isDouble ? Storage.Type.DOUBLE : Storage.Type.LONG;
  }

  @Override
  public void appendNoGrow(Object o) {
    if (o == null) {
      isMissing.set(currentSize++);
    } else if (isDouble) {
      double value = NumericConverter.coerceToDouble(o);
      data[currentSize++] = Double.doubleToRawLongBits(value);
    } else {
      data[currentSize++] = NumericConverter.coerceToLong(o);
    }
  }

  @Override
  public boolean accepts(Object o) {
    if (isDouble) {
      return NumericConverter.isCoercibleToDouble(o);
    } else {
      return NumericConverter.isCoercibleToLong(o);
    }
  }

  @Override
  public void append(Object o) {
    if (currentSize + 1 > data.length) {
      grow();
    }
    appendNoGrow(o);
  }

  @Override
  public void appendNulls(int count) {
    isMissing.set(currentSize, currentSize + count);
    currentSize += count;
  }

  @Override
  public void appendBulkStorage(Storage<?> storage) {
     if (isDouble) {
       appendBulkDouble(storage);
     } else {
        appendBulkLong(storage);
     }
  }

  private void ensureFreeSpaceFor(int additionalSize) {
    if (currentSize + additionalSize > data.length) {
      grow(currentSize + additionalSize);
    }
  }

  private void appendBulkDouble(Storage<?> storage) {
    if (storage.getType() == Storage.Type.DOUBLE) {
      if (storage instanceof DoubleStorage doubleStorage) {
        int n = doubleStorage.size();
        ensureFreeSpaceFor(n);
        System.arraycopy(doubleStorage.getRawData(), 0, data, currentSize, n);
        BitSets.copy(doubleStorage.getIsMissing(), isMissing, currentSize, n);
        currentSize += n;
      } else {
        throw new IllegalStateException("Unexpected storage implementation for type DOUBLE: " + storage + ". This is a bug in the Table library.");
      }
    } else if (storage.getType() == Storage.Type.LONG) {
      if (storage instanceof LongStorage longStorage) {
        int n = longStorage.size();
        BitSets.copy(longStorage.getIsMissing(), isMissing, currentSize, n);
        for (int i = 0; i < n; i++) {
          data[currentSize++] = Double.doubleToRawLongBits(longStorage.getItem(i));
        }
      } else {
        throw new IllegalStateException("Unexpected storage implementation for type LONG: " + storage + ". This is a bug in the Table library.");
      }
    } else if (storage.getType() == Storage.Type.BOOL) {
      if (storage instanceof BoolStorage boolStorage) {
        int n = boolStorage.size();
        for (int i = 0; i < n; i++) {
          if (boolStorage.isNa(i)) {
            isMissing.set(currentSize++);
          } else {
            double x = booleanAsDouble(boolStorage.getItem(i));
            data[currentSize++] = Double.doubleToRawLongBits(x);
          }
        }
      } else {
        throw new IllegalStateException("Unexpected storage implementation for type BOOLEAN: " + storage + ". This is a bug in the Table library.");
      }
    } else {
      throw new StorageTypeMismatch(getType(), storage.getType());
    }
  }

  private void appendBulkLong(Storage<?> storage) {
    if (storage.getType() == Storage.Type.LONG) {
      if (storage instanceof LongStorage longStorage) {
        int n = longStorage.size();
        ensureFreeSpaceFor(n);
        System.arraycopy(longStorage.getRawData(), 0, data, currentSize, n);
        BitSets.copy(longStorage.getIsMissing(), isMissing, currentSize, n);
        currentSize += n;
      } else {
        throw new IllegalStateException("Unexpected storage implementation for type DOUBLE: " + storage + ". This is a bug in the Table library.");
      }
    } else if (storage.getType() == Storage.Type.BOOL) {
      if (storage instanceof BoolStorage boolStorage) {
        int n = boolStorage.size();
        for (int i = 0; i < n; i++) {
          if (boolStorage.isNa(i)) {
            isMissing.set(currentSize++);
          } else {
            data[currentSize++] = booleanAsLong(boolStorage.getItem(i));
          }
        }
      } else {
        throw new IllegalStateException("Unexpected storage implementation for type BOOLEAN: " + storage + ". This is a bug in the Table library.");
      }
    } else {
      throw new StorageTypeMismatch(getType(), storage.getType());
    }
  }

  private long booleanAsLong(boolean value) {
    return value ? 1 : 0;
  }

  private double booleanAsDouble(boolean value) {
    return value ? 1.0 : 0.0;
  }

  /**
   * Append a new item in raw form to this builder, assuming that it has enough allocated space.
   *
   * <p>This function should only be used when it is guaranteed that the builder has enough
   * capacity, for example if it was initialized with an initial capacity known up-front.
   *
   * @param rawData the raw encoding of the item, for long numbers just the number and for doubles,
   *     its long bytes
   */
  public void appendRawNoGrow(long rawData) {
    data[currentSize++] = rawData;
  }

  /**
   * Append a new integer to this builder.
   *
   * @param data the integer to append
   */
  public void appendLong(long data) {
    if (currentSize + 1 > this.data.length) {
      grow();
    }
    appendRawNoGrow(data);
  }

  /**
   * Append a new double to this builder.
   *
   * @param data the double to append
   */
  public void appendDouble(double data) {
    if (currentSize + 1 > this.data.length) {
      grow();
    }
    appendRawNoGrow(Double.doubleToRawLongBits(data));
  }

  @Override
  public int getCurrentSize() {
    return currentSize;
  }

  @Override
  public Storage<?> seal() {
    if (isDouble) {
      return new DoubleStorage(data, currentSize, isMissing);
    } else {
      return new LongStorage(data, currentSize, isMissing);
    }
  }

  private void grow() {
    int desiredCapacity = 3;
    if (data.length > 1) {
      desiredCapacity = (data.length * 3 / 2);
    }
    grow(desiredCapacity);
  }

  private void grow(int desiredCapacity) {
    this.data = Arrays.copyOf(data, desiredCapacity);
  }
}
