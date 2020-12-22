package org.enso.table.data.column.builder.object;

import java.util.BitSet;
import org.enso.table.data.column.storage.DoubleStorage;
import org.enso.table.data.column.storage.LongStorage;
import org.enso.table.data.column.storage.Storage;

/** A builder for numeric columns. */
public class NumericBuilder extends TypedBuilder {
  private final int size;
  private final BitSet isMissing = new BitSet();
  private final long[] data;
  private boolean isDouble;
  private int currentSize;

  private NumericBuilder(boolean isDouble, int size) {
    this.size = size;
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
  public void append(Object o) {
    if (o == null) {
      isMissing.set(currentSize++);
    } else if (isDouble && o instanceof Double) {
      data[currentSize++] = Double.doubleToRawLongBits((Double) o);
    } else if (!isDouble && o instanceof Long) {
      data[currentSize++] = (Long) o;
    } else {
      throw new UnsupportedOperationException();
    }
  }

  /**
   * Append a new item in raw form to this builder.
   *
   * @param rawData the raw encoding of the item, for long numbers just the number and for doubles,
   *     its long bytes
   */
  public void appendRaw(long rawData) {
    data[currentSize++] = rawData;
  }

  @Override
  public int getCurrentSize() {
    return currentSize;
  }

  @Override
  public Storage seal() {
    if (isDouble) {
      return new DoubleStorage(data, size, isMissing);
    } else {
      return new LongStorage(data, size, isMissing);
    }
  }
}
