package org.enso.table.data.column.builder;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Objects;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.ValueTypeMismatchException;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.BitSets;

/** A builder for floating point columns. */
public class DoubleBuilder extends NumericBuilder implements BuilderForDouble {
  protected final PrecisionLossAggregator precisionLossAggregator;
  protected double[] data;

  DoubleBuilder(int initialSize, ProblemAggregator problemAggregator) {
    super();
    this.data = new double[initialSize];
    precisionLossAggregator = new PrecisionLossAggregator(problemAggregator);
  }

  @Override
  protected int getDataSize() {
    return data.length;
  }

  @Override
  protected void resize(int desiredCapacity) {
    double[] newData = new double[desiredCapacity];
    int toCopy = Math.min(currentSize, data.length);
    System.arraycopy(data, 0, newData, 0, toCopy);
    data = newData;
  }

  @Override
  public void copyDataTo(Object[] items) {
    throw new UnsupportedOperationException(
        "The DoubleBuilder cannot be copied to Object[], because it would lose type"
            + " information about integers that were converted to doubles. If recasting is needed,"
            + " InferredDoubleBuilder should be used instead. This error leaking is a bug in the"
            + " Table library.");
  }

  @Override
  public StorageType getType() {
    return FloatType.FLOAT_64;
  }

  @Override
  public void appendNoGrow(Object o) {
    if (o == null) {
      isNothing.set(currentSize++);
      return;
    }

    double value;
    if (NumericConverter.isFloatLike(o)) {
      value = NumericConverter.coerceToDouble(o);
    } else if (NumericConverter.isCoercibleToLong(o)) {
      long longValue = NumericConverter.coerceToLong(o);
      value = convertLongToDouble(longValue);
    } else if (o instanceof BigInteger bigInteger) {
      value = convertBigIntegerToDouble(bigInteger);
    } else if (o instanceof BigDecimal bigDecimal) {
      value = convertBigDecimalToDouble(bigDecimal);
    } else {
      throw new ValueTypeMismatchException(getType(), o);
    }

    data[currentSize++] = value;
  }

  @Override
  public void appendBulkStorage(Storage<?> storage) {
    if (Objects.equals(storage.getType(), FloatType.FLOAT_64)) {
      if (storage instanceof DoubleStorage doubleStorage) {
        int n = doubleStorage.size();
        ensureFreeSpaceFor(n);
        System.arraycopy(doubleStorage.getRawData(), 0, data, currentSize, n);
        BitSets.copy(doubleStorage.getIsNothingMap(), isNothing, currentSize, n);
        currentSize += n;
      } else {
        throw new IllegalStateException(
            "Unexpected storage implementation for type DOUBLE: "
                + storage
                + ". This is a bug in the Table library.");
      }
    } else if (storage.getType() instanceof IntegerType) {
      if (storage instanceof AbstractLongStorage longStorage) {
        int n = longStorage.size();
        BitSets.copy(longStorage.getIsNothingMap(), isNothing, currentSize, n);
        for (int i = 0; i < n; i++) {
          long item = longStorage.getItem(i);
          data[currentSize++] = convertLongToDouble(item);
        }
      } else {
        throw new IllegalStateException(
            "Unexpected storage implementation for type INTEGER: "
                + storage
                + ". This is a bug in the Table library.");
      }
    } else if (storage.getType() instanceof BigIntegerType) {
      if (storage instanceof BigIntegerStorage bigIntegerStorage) {
        int n = bigIntegerStorage.size();
        for (int i = 0; i < n; i++) {
          BigInteger item = bigIntegerStorage.getItem(i);
          if (item == null) {
            isNothing.set(currentSize++);
          } else {
            data[currentSize++] = convertBigIntegerToDouble(item);
          }
        }
      } else {
        throw new IllegalStateException(
            "Unexpected storage implementation for type BIG INTEGER: "
                + storage
                + ". This is a bug in the Table library.");
      }
    } else if (storage.getType() instanceof BooleanType) {
      if (storage instanceof BoolStorage boolStorage) {
        int n = boolStorage.size();
        for (int i = 0; i < n; i++) {
          if (boolStorage.isNothing(i)) {
            isNothing.set(currentSize++);
          } else {
            data[currentSize++] = boolStorage.getItem(i) ? 1.0 : 0.0;
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
   * Append a new double to this builder.
   *
   * @param value the double to append
   */
  public void appendDouble(double value) {
    if (currentSize >= data.length) {
      grow();
    }
    data[currentSize++] = value;
  }

  /**
   * Append a new integer value to this builder, converting it to a double value.
   *
   * <p>It ensures that any loss of precision is reported.
   */
  public void appendLong(long value) {
    appendDouble(convertLongToDouble(value));
  }

  @Override
  public Storage<Double> seal() {
    return new DoubleStorage(data, currentSize, isNothing);
  }

  /**
   * Converts and `long` value into `double`.
   *
   * <p>It verifies if the integer can be exactly represented in a double, and if not, it reports a
   * warning.
   */
  protected double convertLongToDouble(long integer) {
    double floatingPointValue = (double) integer;
    boolean isLosingPrecision = (long) floatingPointValue != integer;
    if (isLosingPrecision) {
      precisionLossAggregator.reportIntegerPrecisionLoss(integer, floatingPointValue);
    }
    return floatingPointValue;
  }

  protected double convertBigIntegerToDouble(BigInteger bigInteger) {
    double floatingPointValue = bigInteger.doubleValue();
    BigInteger reconstructed = BigDecimal.valueOf(floatingPointValue).toBigInteger();
    boolean isLosingPrecision = !bigInteger.equals(reconstructed);
    if (isLosingPrecision) {
      precisionLossAggregator.reportIntegerPrecisionLoss(bigInteger, floatingPointValue);
    }
    return floatingPointValue;
  }

  protected double convertBigDecimalToDouble(BigDecimal bigDecimal) {
    double floatingPointValue = bigDecimal.doubleValue();
    if (Double.isInfinite(floatingPointValue)) {
      precisionLossAggregator.reportBigDecimalPrecisionLoss(bigDecimal, floatingPointValue);
    } else {
      BigDecimal reconstructed = BigDecimal.valueOf(floatingPointValue);
      boolean isLosingPrecision = !bigDecimal.equals(reconstructed);
      if (isLosingPrecision) {
        precisionLossAggregator.reportBigDecimalPrecisionLoss(bigDecimal, floatingPointValue);
      }
    }
    return floatingPointValue;
  }

  protected static class PrecisionLossAggregator extends ProblemAggregator {
    protected PrecisionLossAggregator(ProblemAggregator parent) {
      super(parent);
    }

    private LossOfIntegerPrecision integerInstance = null;
    private LossOfBigDecimalPrecision bigDecimalInstance = null;

    @Override
    public ProblemSummary summarize() {
      ProblemSummary summary = super.summarize();
      if (integerInstance != null) {
        summary.add(integerInstance);
      }
      if (bigDecimalInstance != null) {
        summary.add(bigDecimalInstance);
      }
      return summary;
    }

    final void reportIntegerPrecisionLoss(Number number, double approximation) {
      if (integerInstance == null) {
        integerInstance = new LossOfIntegerPrecision(number, approximation);
      } else {
        integerInstance.incrementAffectedRows();
      }
    }

    final void reportBigDecimalPrecisionLoss(BigDecimal number, double approximation) {
      if (bigDecimalInstance == null) {
        bigDecimalInstance = new LossOfBigDecimalPrecision(number, approximation);
      } else {
        bigDecimalInstance.incrementAffectedRows();
      }
    }
  }
}
