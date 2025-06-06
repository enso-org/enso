package org.enso.table.data.column.storage.numeric;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.operation.CachedPropertyCheck;
import org.enso.table.data.column.operation.RequiresNumberFormatting;
import org.enso.table.data.column.storage.PreciseTypeOptions;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;

public class BigIntegerStorage extends SpecializedStorage<BigInteger>
    implements NumericFormattingStorage {
  private final CachedPropertyCheck<Boolean> isNumericFormatRequired;

  /**
   * @param data the underlying data
   */
  public BigIntegerStorage(BigInteger[] data) {
    super(BigIntegerType.INSTANCE, data);

    isNumericFormatRequired =
        new CachedPropertyCheck<>(() -> RequiresNumberFormatting.compute(this, null), false);
  }

  @Override
  protected SpecializedStorage<BigInteger> newInstance(BigInteger[] data) {
    return new BigIntegerStorage(data);
  }

  @Override
  protected BigInteger[] newUnderlyingArray(int size) {
    return new BigInteger[size];
  }

  private long cachedMaxPrecisionStored = -1;

  public long getMaxPrecisionStored() {
    if (cachedMaxPrecisionStored < 0) {
      long maxPrecision = 0;
      for (int i = 0; i < getSize(); i++) {
        BigInteger value = data[i];
        if (value == null) {
          continue;
        }

        BigDecimal asDecimal = new BigDecimal(value);
        assert asDecimal.scale() == 0;
        int precision = asDecimal.precision();
        if (precision > maxPrecision) {
          maxPrecision = precision;
        }
      }

      cachedMaxPrecisionStored = maxPrecision;
    }

    return cachedMaxPrecisionStored;
  }

  @Override
  public StorageType<?> inferPreciseType(PreciseTypeOptions options) {
    StorageType<?> preciseType = inferAndCacheDefaultPreciseType();

    // If shrinking was requested we may shrink INT_64 further
    if (options.shrinkIntegers() && preciseType instanceof IntegerType) {
      return findSmallestIntegerTypeThatFits(options);
    }

    return preciseType;
  }

  private StorageType<?> inferredType = null;

  private StorageType<?> inferAndCacheDefaultPreciseType() {
    if (inferredType == null) {
      boolean allFitInLong = true;
      int visitedCount = 0;

      for (int i = 0; i < getSize(); i++) {
        BigInteger value = data[i];
        if (value == null) {
          continue;
        }

        visitedCount++;
        boolean fitsInLong = IntegerType.INT_64.fits(value);
        if (!fitsInLong) {
          allFitInLong = false;
          break;
        }
      }

      inferredType =
          (allFitInLong && visitedCount > 0) ? IntegerType.INT_64 : BigIntegerType.INSTANCE;
    }

    return inferredType;
  }

  private StorageType<?> findSmallestIntegerTypeThatFits(PreciseTypeOptions options) {
    // This method assumes that all values _do_ fit in some integer type.
    assert inferredType instanceof IntegerType;

    final BigIntegerStorage parent = this;

    // We create a Long storage that gets values by converting our storage.
    ComputedNullableLongStorage longAdapter =
        new ComputedNullableLongStorage((int) getSize()) {
          @Override
          protected Long computeItem(long idx) {
            BigInteger bigInteger = parent.getItemBoxed(idx);
            if (bigInteger == null) {
              return null;
            }

            return bigInteger.longValueExact();
          }
        };

    // And rely on its shrinking logic.
    return longAdapter.inferPreciseType(options);
  }

  /**
   * Checks if any numbers are large enough for the column to require formatin in the table viz.
   *
   * @return true/false if formatting is required
   */
  @Override
  public Boolean cachedNumericFormatCheck() throws InterruptedException {
    return isNumericFormatRequired.get();
  }
}
