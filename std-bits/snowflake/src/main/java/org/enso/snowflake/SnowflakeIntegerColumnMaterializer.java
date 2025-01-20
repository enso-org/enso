package org.enso.snowflake;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.BitSet;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.ValueTypeMismatchException;
import org.graalvm.polyglot.Context;

public class SnowflakeIntegerColumnMaterializer implements Builder {
  private static final BigInteger LONG_MIN = BigInteger.valueOf(Long.MIN_VALUE);
  private static final BigInteger LONG_MAX = BigInteger.valueOf(Long.MAX_VALUE);
  // We start in integer mode and will switch to BigInteger mode if we encounter a value that
  // exceeds the range
  private long[] ints;
  private BitSet intsMissing;
  private BigInteger[] bigInts;
  private int currentSize;
  private Mode mode;

  public SnowflakeIntegerColumnMaterializer(int initialCapacity) {
    ints = new long[initialCapacity];
    intsMissing = new BitSet();
    bigInts = null;
    currentSize = 0;
    mode = Mode.LONG;
  }

  private void retypeToBigIntegers() {
    assert mode == Mode.LONG;
    Context context = Context.getCurrent();
    bigInts = new BigInteger[ints.length];
    for (int i = 0; i < currentSize; i++) {
      if (intsMissing.get(i)) {
        bigInts[i] = null;
      } else {
        bigInts[i] = BigInteger.valueOf(ints[i]);
      }

      context.safepoint();
    }

    ints = null;
    intsMissing = null;
    mode = Mode.BIG_INTEGER;
  }

  private boolean fitsInLong(BigInteger bigInteger) {
    return bigInteger.compareTo(LONG_MIN) >= 0 && bigInteger.compareTo(LONG_MAX) <= 0;
  }

  @Override
  public void appendNoGrow(Object o) {
    switch (o) {
      case BigInteger bigInteger -> {
        switch (mode) {
          case BIG_INTEGER -> bigInts[currentSize++] = bigInteger;

          case LONG -> {
            if (fitsInLong(bigInteger)) {
              ints[currentSize++] = bigInteger.longValue();
            } else {
              retypeToBigIntegers();
              bigInts[currentSize++] = bigInteger;
            }
          }
        }
      }

      case null -> appendNulls(1);
      default -> throw new ValueTypeMismatchException(BigIntegerType.INSTANCE, o);
    }
  }

  @Override
  public void append(Object o) {
    if (currentSize >= capacity()) {
      grow();
    }

    appendNoGrow(o);
  }

  @Override
  public void appendNulls(int count) {
    if (mode == Mode.LONG) {
      intsMissing.set(currentSize, currentSize + count);
    }

    currentSize += count;
  }

  @Override
  public void appendBulkStorage(Storage<?> storage) {
    throw new IllegalStateException(
        "SnowflakeIntegerColumnMaterializer.appendBulkStorage: Not supported.");
  }

  @Override
  public int getCurrentSize() {
    return currentSize;
  }

  @Override
  public Storage<?> seal() {
    resize(currentSize);
    return switch (mode) {
      case LONG -> new LongStorage(ints, currentSize, intsMissing, IntegerType.INT_64);
      case BIG_INTEGER -> new BigIntegerStorage(bigInts, currentSize);
    };
  }

  @Override
  public StorageType getType() {
    // The type of the builder can change over time, so we do not report any stable type here.
    return null;
  }

  @Override
  public void copyDataTo(Object[] items) {
    if (currentSize > 0) {
      if (mode == Mode.LONG) {
        for (int i = 0; i < currentSize; i++) {
          if (intsMissing.get(i)) {
            items[i] = null;
          } else {
            items[i] = ints[i];
          }
        }
      } else {
        System.arraycopy(bigInts, 0, items, 0, currentSize);
      }
    }
  }

  private int capacity() {
    return mode == Mode.LONG ? ints.length : bigInts.length;
  }

  private void grow() {
    int desiredCapacity = 3;
    if (capacity() > 1) {
      desiredCapacity = (capacity() * 3 / 2);
    }

    // It is possible for the `currentSize` to grow arbitrarily larger than
    // the capacity, because when nulls are being added the array is not
    // resized, only the counter is incremented. Thus, we need to ensure
    // that we have allocated enough space for at least one element.
    if (currentSize >= desiredCapacity) {
      desiredCapacity = currentSize + 1;
    }

    resize(desiredCapacity);
  }

  private void resize(int desiredCapacity) {
    switch (mode) {
      case LONG -> ints = Arrays.copyOf(ints, desiredCapacity);
      case BIG_INTEGER -> bigInts = Arrays.copyOf(bigInts, desiredCapacity);
    }
  }

  private enum Mode {
    LONG,
    BIG_INTEGER
  }
}
