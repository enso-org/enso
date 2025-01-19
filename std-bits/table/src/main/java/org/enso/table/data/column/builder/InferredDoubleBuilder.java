package org.enso.table.data.column.builder;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.BitSet;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.ValueTypeMismatchException;
import org.enso.table.problems.ProblemAggregator;

/** A double builder variant that preserves types and can be retyped to Mixed. */
public class InferredDoubleBuilder extends DoubleBuilder implements BuilderWithRetyping {
  /**
   * Converts the provided LongBuilder to a DoubleBuilder.
   *
   * <p>The original LongBuilder becomes invalidated after this operation and should no longer be
   * used.
   */
  static InferredDoubleBuilder retypeFromLongBuilder(LongBuilder longBuilder) {
    int currentSize = longBuilder.currentSize;
    var newBuilder =
        new InferredDoubleBuilder(longBuilder.getDataSize(), longBuilder.problemAggregator);

    for (int i = 0; i < currentSize; i++) {
      newBuilder.appendLongNoGrow(longBuilder.data[i]);
    }
    newBuilder.isNothing = longBuilder.isNothing;

    return newBuilder;
  }

  /**
   * Stores the raw data as passed to append, in order to be able to reconstruct the original values
   * when retyping to mixed.
   */
  private Number[] rawData;

  /**
   * Specifies at which indices we encountered integers that can be reconstructed from double
   * without loss of precision.
   *
   * <p>This is used for reconstructing the original values when retyping to mixed. Integers that
   * are small enough can be reconstructed from their double values, so we do not need to store them
   * as `rawData`, instead we only mark that they need to be converted back into integers when
   * retyping. This allows us to completely avoid allocating the `rawData` array for most practical
   * scenarios when the integers are not too large. The cost of allocating this BitSet should be
   * significantly lower than allocating the `rawData` array.
   */
  private final BitSet isLongCompactedAsDouble;

  InferredDoubleBuilder(int initialSize, ProblemAggregator problemAggregator) {
    super(initialSize, problemAggregator);
    rawData = null;
    isLongCompactedAsDouble = new BitSet();
  }

  @Override
  public void copyDataTo(Object[] items) {
    int rawN = rawData == null ? 0 : rawData.length;
    for (int i = 0; i < currentSize; i++) {
      if (isNothing.get(i)) {
        items[i] = null;
      } else {
        if (isLongCompactedAsDouble.get(i)) {
          items[i] = (long) data[i];
        } else if (i < rawN && rawData[i] != null) {
          items[i] = rawData[i];
        } else {
          items[i] = data[i];
        }
      }
    }

    // Since we are retyping to Mixed, the precision loss warnings should not be inherited - thus we
    // discard them.
    precisionLossAggregator.detachFromParent();
  }

  @Override
  public void appendBulkStorage(Storage<?> storage) {
    throw new UnsupportedOperationException(
        "appendBulkStorage is not supported on InferredDoubleBuilder. A DoubleBuilder or"
            + " MixedBuilder should be used instead. This is a bug in the Table library.");
  }

  @Override
  public void appendLong(long value) {
    super.appendLong(value);
  }

  private void appendLongNoGrow(long integer) {
    double convertedFloatValue = (double) integer;
    boolean isLossy = integer != (long) convertedFloatValue;
    if (isLossy) {
      setRaw(currentSize, integer);
      precisionLossAggregator.reportIntegerPrecisionLoss(integer, convertedFloatValue);
    } else {
      isLongCompactedAsDouble.set(currentSize, true);
    }

    data[currentSize++] = convertedFloatValue;
  }

  @Override
  public void appendNoGrow(Object o) {
    if (o == null) {
      isNothing.set(currentSize++);
      return;
    }

    if (NumericConverter.isFloatLike(o)) {
      data[currentSize++] = NumericConverter.coerceToDouble(o);
    } else if (NumericConverter.isCoercibleToLong(o)) {
      appendLongNoGrow(NumericConverter.coerceToLong(o));
    } else if (o instanceof BigInteger bigInteger) {
      setRaw(currentSize, bigInteger);
      data[currentSize++] = convertBigIntegerToDouble(bigInteger);
    } else if (o instanceof BigDecimal bigDecimal) {
      setRaw(currentSize, bigDecimal);
      data[currentSize++] = convertBigDecimalToDouble(bigDecimal);
    } else {
      throw new ValueTypeMismatchException(getType(), o);
    }
  }

  private void setRaw(int ix, Number o) {
    if (rawData == null) {
      rawData = new Number[ix + 1];
    }

    if (rawData.length <= ix) {
      int newLength = Math.max(rawData.length * 3 / 2 + 1, ix + 1);
      Number[] newRawData = new Number[newLength];
      System.arraycopy(rawData, 0, newRawData, 0, rawData.length);
      rawData = newRawData;
    }

    rawData[ix] = o;
  }

  @Override
  public boolean accepts(Object o) {
    return NumericConverter.isCoercibleToDouble(o);
  }

  @Override
  public boolean canRetypeTo(StorageType type) {
    return type instanceof BigDecimalType;
  }

  @Override
  public Builder retypeTo(StorageType type) {
    if (type instanceof BigDecimalType) {
      Builder res = Builder.getForType(BigDecimalType.INSTANCE, data.length, null);
      for (int i = 0; i < currentSize; i++) {
        if (isNothing.get(i)) {
          res.appendNulls(1);
        } else {
          BigDecimal bigDecimal = BigDecimal.valueOf(data[i]);
          res.appendNoGrow(bigDecimal);
        }
      }
      return res;
    } else {
      throw new UnsupportedOperationException();
    }
  }
}
