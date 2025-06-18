package org.enso.table.data.column.storage.numeric;

import java.math.BigDecimal;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;

public final class BigDecimalStorage extends SpecializedStorage<BigDecimal> {
  /**
   * @param data the underlying data
   */
  public BigDecimalStorage(BigDecimal[] data) {
    super(BigDecimalType.INSTANCE, data);
  }

  @Override
  protected SpecializedStorage<BigDecimal> newInstance(BigDecimal[] data) {
    return new BigDecimalStorage(data);
  }

  @Override
  protected BigDecimal[] newUnderlyingArray(int size) {
    return new BigDecimal[size];
  }
}
