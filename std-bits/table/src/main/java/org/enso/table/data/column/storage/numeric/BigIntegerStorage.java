package org.enso.table.data.column.storage.numeric;

import org.enso.table.data.column.builder.BigIntegerBuilder;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.storage.ObjectStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.StorageType;

import java.math.BigInteger;

public class BigIntegerStorage extends SpecializedStorage<BigInteger> {
  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public BigIntegerStorage(BigInteger[] data, int size) {
    super(data, size, makeOps());
  }

  protected static MapOperationStorage<BigInteger, SpecializedStorage<BigInteger>> makeOps() {
    return ObjectStorage.buildObjectOps();
  }

  @Override
  protected SpecializedStorage<BigInteger> newInstance(BigInteger[] data, int size) {
    return new BigIntegerStorage(data, size);
  }

  @Override
  protected BigInteger[] newUnderlyingArray(int size) {
    return new BigInteger[0];
  }

  @Override
  public StorageType getType() {
    return BigIntegerType.INSTANCE;
  }

  @Override
  public Builder createDefaultBuilderOfSameType(int capacity) {
    return new BigIntegerBuilder(capacity);
  }
}
