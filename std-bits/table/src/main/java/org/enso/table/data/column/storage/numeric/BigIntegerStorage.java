package org.enso.table.data.column.storage.numeric;

import java.math.BigInteger;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.BigIntegerType;

public class BigIntegerStorage extends SpecializedStorage<BigInteger> {
  /**
   * @param data the underlying data
   */
  public BigIntegerStorage(BigInteger[] data) {
    super(BigIntegerType.INSTANCE, data);
  }
}
