package org.enso.table.data.column.storage.numeric;

import java.math.BigDecimal;
import org.enso.table.data.column.operation.CachedPropertyCheck;
import org.enso.table.data.column.operation.RequiresNumberFormatting;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;

public final class BigDecimalStorage extends SpecializedStorage<BigDecimal>
    implements NumericFormattingStorage {
  private final CachedPropertyCheck<Boolean> isNumericFormatRequired;

  /**
   * @param data the underlying data
   */
  public BigDecimalStorage(BigDecimal[] data) {
    super(BigDecimalType.INSTANCE, data);
    isNumericFormatRequired =
        new CachedPropertyCheck<>(() -> RequiresNumberFormatting.compute(this, null), false);
  }

  @Override
  protected SpecializedStorage<BigDecimal> newInstance(BigDecimal[] data) {
    return new BigDecimalStorage(data);
  }

  @Override
  protected BigDecimal[] newUnderlyingArray(int size) {
    return new BigDecimal[size];
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
