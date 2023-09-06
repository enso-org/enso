package org.enso.table.data.column.operation.map.numeric.comparisons;

import java.math.BigInteger;
import org.enso.table.data.column.storage.Storage;

public class GreaterOrEqualComparison<T extends Number, I extends Storage<? super T>>
    extends NumericComparison<T, I> {
  public GreaterOrEqualComparison() {
    super(Storage.Maps.GTE);
  }

  @Override
  protected boolean doDouble(double a, double b) {
    return a >= b;
  }

  @Override
  protected boolean doLong(long a, long b) {
    return a >= b;
  }

  @Override
  protected boolean doBigInteger(BigInteger a, BigInteger b) {
    return a.compareTo(b) >= 0;
  }
}
