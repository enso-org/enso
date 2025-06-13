package org.enso.table.data.column.storage.numeric;

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
