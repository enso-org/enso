package org.enso.table.data.column.builder;

import java.math.BigInteger;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.problems.ProblemAggregator;

/**
 * A builder for storing enso Integers, which might be Longs or BigIntegers.
 *
 * <p>This builder starts off delegating to LongBuilder, but if it receives a BigInteger, it retypes
 * the LongBuilder to a BigIntegerBuilder.
 */
public class InferredIntegerBuilder extends Builder {
  private LongBuilder longBuilder = null;
  private TypedBuilder bigIntegerBuilder = null;
  private int currentSize = 0;
  private final int initialSize;
  private final ProblemAggregator problemAggregator;

  /** Creates a new instance of this builder, with the given known result length. */
  public InferredIntegerBuilder(int initialSize, ProblemAggregator problemAggregator) {
    this.initialSize = initialSize;
    this.problemAggregator = problemAggregator;

    longBuilder =
        NumericBuilder.createLongBuilder(this.initialSize, IntegerType.INT_64, problemAggregator);
  }

  @Override
  public void appendNoGrow(Object o) {
    if (o == null) {
      appendNulls(1);
    } else if (o instanceof BigInteger bi) {
      retypeToBigIntegerMaybe();
      bigIntegerBuilder.appendNoGrow(bi);
    } else {
      Long lng = NumericConverter.tryConvertingToLong(o);
      if (lng == null) {
        throw new IllegalStateException(
            "Unexpected value added to InferredIntegerBuilder "
                + o.getClass()
                + ". This is a bug in the Table library.");
      } else {
        if (bigIntegerBuilder != null) {
          bigIntegerBuilder.appendNoGrow(BigInteger.valueOf(lng));
        } else {
          longBuilder.appendNoGrow(lng);
        }
      }
    }
    currentSize++;
  }

  @Override
  public void append(Object o) {
    if (o == null) {
      appendNulls(1);
    } else if (o instanceof BigInteger bi) {
      retypeToBigIntegerMaybe();
      bigIntegerBuilder.append(bi);
    } else {
      Long lng = NumericConverter.tryConvertingToLong(o);
      if (lng == null) {
        throw new IllegalStateException(
            "Unexpected value added to InferredIntegerBuilder "
                + o.getClass()
                + ". This is a bug in the Table library.");
      } else {
        if (bigIntegerBuilder != null) {
          bigIntegerBuilder.append(BigInteger.valueOf(lng));
        } else {
          longBuilder.append(lng);
        }
      }
    }
    currentSize++;
  }

  @Override
  public void appendNulls(int count) {
    if (bigIntegerBuilder != null) {
      bigIntegerBuilder.appendNulls(count);
    } else {
      longBuilder.appendNulls(count);
    }
    currentSize += count;
  }

  @Override
  public void appendBulkStorage(Storage<?> storage) {
    for (int i = 0; i < storage.size(); i++) {
      append(storage.getItemBoxed(i));
    }
  }

  @Override
  public int getCurrentSize() {
    return currentSize;
  }

  @Override
  public Storage<?> seal() {
    if (bigIntegerBuilder != null) {
      return bigIntegerBuilder.seal();
    } else {
      return longBuilder.seal();
    }
  }

  @Override
  public StorageType getType() {
    if (bigIntegerBuilder != null) {
      return BigIntegerType.INSTANCE;
    } else {
      return IntegerType.INT_64;
    }
  }

  // Retype the LongBuilder to a BigIntegerBuilder, if we haven't already
  // done so.
  private void retypeToBigIntegerMaybe() {
    if (bigIntegerBuilder != null) {
      return;
    }
    bigIntegerBuilder = longBuilder.retypeTo(BigIntegerType.INSTANCE);
    longBuilder = null;
  }
}
