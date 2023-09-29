package org.enso.table.data.column.builder;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.error.ValueTypeMismatchException;

import java.math.BigInteger;
import java.util.BitSet;

/**
 * A double builder variant that preserves types and can be retyped to Mixed.
 */
public class InferringDoubleBuilder extends DoubleBuilder {
  /**
   * Converts the provided LongBuilder to a DoubleBuilder.
   *
   * <p>The original LongBuilder becomes invalidated after this operation and should no longer be
   * used.
   */
  static InferringDoubleBuilder retypeFromLongBuilder(LongBuilder longBuilder) {
    int currentSize = longBuilder.currentSize;
    InferringDoubleBuilder newBuilder =
        new InferringDoubleBuilder(longBuilder.isMissing, longBuilder.data, currentSize);

    // Invalidate the old builder.
    longBuilder.data = null;
    longBuilder.isMissing = null;
    longBuilder.currentSize = -1;

    // Assume all longs will be compacted.
    newBuilder.isLongCompactedAsDouble.set(0, currentSize, true);

    // Translate the data in-place to avoid unnecessary allocations.
    for (int i = 0; i < currentSize; i++) {
      if (!newBuilder.isMissing.get(i)) {
        long currentIntegerValue = newBuilder.data[i];
        double convertedFloatValue = (double) currentIntegerValue;
        boolean isLossy = currentIntegerValue != (long) convertedFloatValue;
        if (isLossy) {
          // Save it raw for recovery.
          newBuilder.setRaw(i, currentIntegerValue);
          newBuilder.reportPrecisionLoss(currentIntegerValue, convertedFloatValue);
          // Unmark the long that did not fit:
          newBuilder.isLongCompactedAsDouble.set(i, false);
        }

        newBuilder.data[i] = Double.doubleToRawLongBits(convertedFloatValue);
      }
    }

    return newBuilder;
  }

  InferringDoubleBuilder(BitSet isMissing, long[] doubleData, int currentSize) {
    super(isMissing, doubleData, currentSize);
    rawData = null;
    isLongCompactedAsDouble = new BitSet();
  }

  /**
   * Stores the raw data as passed to append, in order to be able to reconstruct the original values when retyping to
   * mixed.
   */
  private Number[] rawData;

  /**
   * Specifies at which indices we encountered integers that can be reconstructed from double without loss of
   * precision.
   * <p>
   * This is used for reconstructing the original values when retyping to mixed. Integers that are small enough can be
   * reconstructed from their double values, so we do not need to store them as `rawData`, instead we only mark that
   * they need to be converted back into integers when retyping. This allows us to completely avoid allocating the
   * `rawData` array for most practical scenarios when the integers are not too large. The cost of allocating this
   * BitSet should be significantly lower than allocating the `rawData` array.
   */
  private final BitSet isLongCompactedAsDouble;

  @Override
  public void retypeToMixed(Object[] items) {
    int rawN = rawData == null ? 0 : rawData.length;
    for (int i = 0; i < currentSize; i++) {
      if (isMissing.get(i)) {
        items[i] = null;
      } else {
        if (isLongCompactedAsDouble.get(i)) {
          double value = Double.longBitsToDouble(data[i]);
          long reconstructed = (long) value;
          items[i] = reconstructed;
        } else if (i < rawN && rawData[i] != null) {
          items[i] = rawData[i];
        } else {
          double value = Double.longBitsToDouble(data[i]);
          items[i] = value;
        }
      }
    }

    // Since we are retyping to Mixed, the precision loss warnings should not be inherited - thus we reset them.
    precisionLoss = null;
  }

  @Override
  public void appendBulkStorage(Storage<?> storage) {
    throw new UnsupportedOperationException("appendBulkStorage is not supported on InferringDoubleBuilder. " +
        "A DoubleBuilder or MixedBuilder should be used instead. This is a bug in the Table library.");
  }

  @Override
  public void appendDouble(double x) {
    if (currentSize >= this.data.length) {
      grow();
    }

    data[currentSize] = Double.doubleToRawLongBits(x);
    currentSize++;
  }

  @Override
  public void appendLong(long integer) {
    if (currentSize >= this.data.length) {
      grow();
    }

    appendLongNoGrow(integer);
  }

  private void appendLongNoGrow(long integer) {
    double convertedFloatValue = (double) integer;
    boolean isLossy = integer != (long) convertedFloatValue;
    if (isLossy) {
      setRaw(currentSize, integer);
      reportPrecisionLoss(integer, convertedFloatValue);
    } else {
      isLongCompactedAsDouble.set(currentSize, true);
    }

    data[currentSize] = Double.doubleToRawLongBits(convertedFloatValue);
    currentSize++;
  }

  @Override
  public void appendBigInteger(BigInteger integer) {
    if (currentSize >= this.data.length) {
      grow();
    }

    setRaw(currentSize, integer);
    double convertedFloatValue = convertBigIntegerToDouble(integer);
    data[currentSize] = Double.doubleToRawLongBits(convertedFloatValue);
    currentSize++;
  }

  @Override
  public void appendNoGrow(Object o) {
    if (o == null) {
      isMissing.set(currentSize++);
    } else if (NumericConverter.isFloatLike(o)) {
      double value = NumericConverter.coerceToDouble(o);
      data[currentSize++] = Double.doubleToRawLongBits(value);
    } else if (NumericConverter.isCoercibleToLong(o)) {
      long value = NumericConverter.coerceToLong(o);
      appendLongNoGrow(value);
    } else if (o instanceof BigInteger bigInteger) {
      setRaw(currentSize, bigInteger);
      double converted = convertBigIntegerToDouble(bigInteger);
      data[currentSize++] = Double.doubleToRawLongBits(converted);
    } else {
      throw new ValueTypeMismatchException(getType(), o);
    }
  }

  @Override
  public void appendRawNoGrow(long rawData) {
    throw new UnsupportedOperationException("appendRawNoGrow is not supported on InferringDoubleBuilder. " +
        "A DoubleBuilder should be used instead. This is a bug in the Table library.");
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
}
