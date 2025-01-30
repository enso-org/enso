package org.enso.table.data.column.operation.map.numeric.comparisons;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.numeric.helpers.DoubleArrayAdapter;
import org.enso.table.data.column.storage.Storage;

public class EqualsComparison<T extends Number, I extends Storage<? super T>>
    extends NumericComparison<T, I> {
  public EqualsComparison() {
    super(Storage.Maps.EQ);
  }

  @Override
  protected boolean doDouble(double a, double b) {
    return a == b;
  }

  @Override
  protected Storage<Boolean> runDoubleMap(
      DoubleArrayAdapter lhs, double rhs, MapOperationProblemAggregator problemAggregator) {
    problemAggregator.reportFloatingPointEquality(-1);
    return super.runDoubleMap(lhs, rhs, problemAggregator);
  }

  @Override
  protected Storage<Boolean> runDoubleZip(
      DoubleArrayAdapter lhs,
      DoubleArrayAdapter rhs,
      MapOperationProblemAggregator problemAggregator) {
    problemAggregator.reportFloatingPointEquality(-1);
    return super.runDoubleZip(lhs, rhs, problemAggregator);
  }

  @Override
  protected boolean doLong(long a, long b) {
    return a == b;
  }

  @Override
  protected boolean doBigInteger(BigInteger a, BigInteger b) {
    return a.equals(b);
  }

  @Override
  protected boolean doBigDecimal(BigDecimal a, BigDecimal b) {
    return a.compareTo(b) == 0;
  }

  @Override
  protected boolean onOtherType(Object a, Object b) {
    return false;
  }
}
