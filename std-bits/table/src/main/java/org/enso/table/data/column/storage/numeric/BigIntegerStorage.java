package org.enso.table.data.column.storage.numeric;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.operation.CachedPropertyCheck;
import org.enso.table.data.column.operation.RequiresNumberFormatting;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.BigIntegerType;

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
