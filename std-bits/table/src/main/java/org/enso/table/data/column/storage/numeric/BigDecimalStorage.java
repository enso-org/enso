package org.enso.table.data.column.storage.numeric;

import java.math.BigDecimal;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.storage.ObjectStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.StorageType;

public final class BigDecimalStorage extends SpecializedStorage<BigDecimal> {
  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public BigDecimalStorage(BigDecimal[] data, int size) {
    super(data, size, buildOps());
  }

  private static MapOperationStorage<BigDecimal, SpecializedStorage<BigDecimal>> buildOps() {
    return ObjectStorage.buildObjectOps();
  }

  @Override
  protected SpecializedStorage<BigDecimal> newInstance(BigDecimal[] data, int size) {
    return new BigDecimalStorage(data, size);
  }

  @Override
  protected BigDecimal[] newUnderlyingArray(int size) {
    return new BigDecimal[size];
  }

  @Override
  public StorageType getType() {
    return BigDecimalType.INSTANCE;
  }
}
