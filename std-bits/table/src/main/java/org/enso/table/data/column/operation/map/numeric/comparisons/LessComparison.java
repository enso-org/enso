package org.enso.table.data.column.operation.map.numeric.comparisons;

import org.enso.table.data.column.storage.Storage;

import java.math.BigInteger;

public class LessComparison<T extends Number, I extends Storage<? super T>> extends NumericComparison<T, I> {
  public LessComparison() {
    super(Storage.Maps.LT);
  }

  @Override
  protected boolean doDouble(double a, double b) {
    return a < b;
  }

  @Override
  protected boolean doLong(long a, long b) {
    return a < b;
  }

  @Override
  protected boolean doBigInteger(BigInteger a, BigInteger b) {
    return a.compareTo(b) < 0;
  }
}
