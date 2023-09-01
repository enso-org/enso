package org.enso.table.data.column.operation.cast;

import org.enso.table.data.column.storage.Storage;

import java.math.BigInteger;

public class ToBigIntegerConverter implements StorageConverter<BigInteger> {
  @Override
  public Storage<BigInteger> cast(Storage<?> storage, CastProblemBuilder problemBuilder) {
    throw new IllegalStateException("TODO");
  }
}
