package org.enso.snowflake;

import java.math.BigInteger;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForLong;
import org.enso.table.data.column.builder.BuilderWithRetyping;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.ValueTypeMismatchException;
import org.enso.table.problems.BlackholeProblemAggregator;

public class SnowflakeIntegerColumnMaterializer implements Builder {
  private static final BigInteger LONG_MIN = BigInteger.valueOf(Long.MIN_VALUE);
  private static final BigInteger LONG_MAX = BigInteger.valueOf(Long.MAX_VALUE);
  // We start in integer mode and will switch to BigInteger mode if we encounter
  // a value that exceeds the range
  private BuilderForLong longBuilder;
  private Builder bigIntegerBuilder;
  private Mode mode;

  public SnowflakeIntegerColumnMaterializer(int initialCapacity) {
    mode = Mode.LONG;
    longBuilder =
        Builder.getForLong(
            IntegerType.INT_64, initialCapacity, BlackholeProblemAggregator.INSTANCE);

    if (!(longBuilder instanceof BuilderWithRetyping withRetyping)
        || !withRetyping.canRetypeTo(BigIntegerType.INSTANCE)) {
      throw new IllegalArgumentException(
          "SnowflakeIntegerColumnMaterializer: Cannot retype to BigIntegerType. This is a bug in"
              + " the Table library.");
    }
  }

  private void retypeToBigIntegers() {
    assert mode == Mode.LONG;

    bigIntegerBuilder = ((BuilderWithRetyping) longBuilder).retypeTo(BigIntegerType.INSTANCE);
    longBuilder = null;
    mode = Mode.BIG_INTEGER;
  }

  private boolean fitsInLong(BigInteger bigInteger) {
    return bigInteger.compareTo(LONG_MIN) >= 0 && bigInteger.compareTo(LONG_MAX) <= 0;
  }

  @Override
  public SnowflakeIntegerColumnMaterializer append(Object o) {
    if (o instanceof BigInteger bigInteger) {
      switch (mode) {
        case BIG_INTEGER -> bigIntegerBuilder.append(bigInteger);
        case LONG -> {
          if (fitsInLong(bigInteger)) {
            longBuilder.append(bigInteger);
          } else {
            retypeToBigIntegers();
            bigIntegerBuilder.append(bigInteger);
          }
        }
      }
    } else {
      throw new ValueTypeMismatchException(BigIntegerType.INSTANCE, o);
    }

    return this;
  }

  @Override
  public SnowflakeIntegerColumnMaterializer appendNulls(int count) {
    if (mode == Mode.LONG) {
      longBuilder.appendNulls(count);
    } else {
      bigIntegerBuilder.appendNulls(count);
    }

    return this;
  }

  @Override
  public void appendBulkStorage(ColumnStorage<?> storage) {
    throw new IllegalStateException(
        "SnowflakeIntegerColumnMaterializer.appendBulkStorage: Not supported.");
  }

  @Override
  public long getCurrentSize() {
    return mode == Mode.BIG_INTEGER
        ? bigIntegerBuilder.getCurrentSize()
        : longBuilder.getCurrentSize();
  }

  @Override
  public ColumnStorage<?> seal() {
    return switch (mode) {
      case LONG -> longBuilder.seal();
      case BIG_INTEGER -> bigIntegerBuilder.seal();
    };
  }

  @Override
  public StorageType<?> getType() {
    // The type of the builder can change over time, so we do not report any stable type here.
    return null;
  }

  @Override
  public void copyDataTo(Object[] items) {
    if (mode == Mode.LONG) {
      longBuilder.copyDataTo(items);
    } else {
      bigIntegerBuilder.copyDataTo(items);
    }
  }

  private enum Mode {
    LONG,
    BIG_INTEGER
  }
}
