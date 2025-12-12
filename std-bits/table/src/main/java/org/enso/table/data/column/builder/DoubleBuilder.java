package org.enso.table.data.column.builder;

import java.lang.foreign.MemorySegment;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.DoubleBuffer;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.DoubleStorage;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.ValueTypeMismatchException;
import org.enso.table.problems.ProblemAggregator;

/** A builder for floating point columns. */
sealed class DoubleBuilder extends NumericBuilder implements BuilderForDouble
    permits InferredDoubleBuilder {
  protected final PrecisionLossAggregator precisionLossAggregator;
  private DoubleBuffer data;

  DoubleBuilder(int initialSize, ProblemAggregator problemAggregator) {
    this(allocBuffer(initialSize, 0L), initialSize, 0L, problemAggregator);
  }

  private DoubleBuilder(
      DoubleBuffer buf, int initialSize, long validity, ProblemAggregator problemAggregator) {
    super(initialSize, validity);
    assert ByteOrder.LITTLE_ENDIAN == buf.order();
    this.data = buf;
    precisionLossAggregator =
        problemAggregator == null ? null : new PrecisionLossAggregator(problemAggregator);
  }

  /**
   * Allocates continuous direct memory buffer.
   *
   * @param size the size of buffer to allocate
   * @param data address of data to read or {@code 0} to allocate new data
   * @return buffer representing data
   */
  private static DoubleBuffer allocBuffer(int size, long data) {
    var wholeDataSize = Double.BYTES * size;
    ByteBuffer buf;
    if (data == 0L) {
      buf = ByteBuffer.allocateDirect(wholeDataSize).order(ByteOrder.LITTLE_ENDIAN);
    } else {
      var seg = MemorySegment.ofAddress(data).reinterpret(wholeDataSize);
      buf = seg.asByteBuffer().order(ByteOrder.LITTLE_ENDIAN);
    }
    assert buf.capacity() == wholeDataSize;
    var doubles = buf.order(ByteOrder.LITTLE_ENDIAN).asDoubleBuffer();
    assert doubles.capacity() == size;
    assert doubles.order() == ByteOrder.LITTLE_ENDIAN;
    return doubles;
  }

  static DoubleBuilder fromAddress(int size, long address, long validity, FloatType type) {
    assert address != 0;
    var buf = allocBuffer(size, address);
    var builder = new DoubleBuilder(buf, size, validity, null);
    return builder;
  }

  @Override
  protected int getDataSize() {
    return data.capacity();
  }

  final double getData(int i) {
    return data.get(i);
  }

  @Override
  protected void resize(int desiredCapacity) {
    var newData = allocBuffer(desiredCapacity, 0);
    int toCopy = Math.min(currentSize, data.capacity());
    newData.put(0, data, 0, toCopy);
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
  public StorageType<Double> getType() {
    return FloatType.FLOAT_64;
  }

  @Override
  public DoubleBuilder appendNulls(int count) {
    doAppendNulls(count);
    return this;
  }

  @Override
  public DoubleBuilder append(Object o) {
    if (o == null) {
      return appendNulls(1);
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

    ensureSpaceToAppend();
    setValid(currentSize);
    data.put(currentSize++, value);
    return this;
  }

  @Override
  public void appendBulkStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof FloatType floatType) {
      if (storage instanceof DoubleStorage doubleStorage) {
        int n = (int) doubleStorage.getSize();
        ensureFreeSpaceFor(n);
        data.put(currentSize, doubleStorage.getData(), 0, n);
        appendValidityMap(doubleStorage.getValidityMap(), n);
        currentSize += n;
      } else {
        var doubleStorage = floatType.asTypedStorage(storage);
        long n = doubleStorage.getSize();
        for (long i = 0; i < n; i++) {
          if (storage.isNothing(i)) {
            appendNulls(1);
          } else {
            appendDouble(doubleStorage.getItemAsDouble(i));
          }
        }
      }
    } else if (storage.getType() instanceof IntegerType integerType) {
      var longStorage = integerType.asTypedStorage(storage);
      long n = longStorage.getSize();
      for (long i = 0; i < n; i++) {
        if (storage.isNothing(i)) {
          appendNulls(1);
        } else {
          long item = longStorage.getItemAsLong(i);
          appendDouble(convertLongToDouble(item));
        }
      }
    } else if (storage.getType() instanceof BigIntegerType bigIntegerType) {
      var bigIntegerStorage = bigIntegerType.asTypedStorage(storage);
      long n = bigIntegerStorage.getSize();
      for (long i = 0; i < n; i++) {
        BigInteger item = bigIntegerStorage.getItemBoxed(i);
        if (item == null) {
          appendNulls(1);
        } else {
          appendDouble(convertBigIntegerToDouble(item));
        }
      }
    } else if (storage instanceof ColumnBooleanStorage boolStorage) {
      long n = boolStorage.getSize();
      for (long i = 0; i < n; i++) {
        if (boolStorage.isNothing(i)) {
          appendNulls(1);
        } else {
          appendDouble(boolStorage.getItemAsBoolean(i) ? 1.0 : 0.0);
        }
      }
    } else if (storage.getType() instanceof NullType) {
      appendNulls(Math.toIntExact(storage.getSize()));
    } else {
      throw new StorageTypeMismatchException(getType(), storage.getType());
    }
  }

  /**
   * Append a new double to this builder.
   *
   * @param value the double to append
   */
  @Override
  public DoubleBuilder appendDouble(double value) {
    ensureSpaceToAppend();
    setValid(currentSize);
    data.put(currentSize++, value);
    return this;
  }

  /**
   * Append a new integer value to this builder, converting it to a double value.
   *
   * <p>It ensures that any loss of precision is reported.
   */
  @Override
  public DoubleBuilder appendLong(long value) {
    appendDouble(convertLongToDouble(value));
    return this;
  }

  @Override
  public ColumnStorage<Double> seal() {
    return seal(null, getType());
  }

  /**
   * Seals this buffer as copy of provided storage.
   *
   * @param otherStorage storage to copy size from if non-{@code null}
   * @param type the type to assign to the created storage
   * @return locally copied storage
   */
  final DoubleStorage seal(ColumnStorage<?> otherStorage, StorageType<Double> type) {
    ensureFreeSpaceFor(0);
    var buf = data.asReadOnlyBuffer().position(0).limit(currentSize);
    var validity = this.validityMap();
    return new DoubleStorage(buf, validity, otherStorage);
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
