package org.enso.table.data.column.operation.map.numeric.arithmetic;

import java.math.BigDecimal;

import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.Storage;

public class DivideOp<T extends Number, I extends Storage<? super T>>
    extends NumericBinaryOpReturningDouble<T, I> {
  public DivideOp() {
    super(Storage.Maps.DIV);
  }

  @Override
  public double doDouble(
      double a, double b, int ix, MapOperationProblemAggregator problemAggregator) {
    if (b == 0.0) {
      problemAggregator.reportDivisionByZero(ix);
    }
    return a / b;
  }

  @Override
  public BigDecimal doBigDecimal(
      BigDecimal a, BigDecimal b, int ix, MapOperationProblemAggregator problemAggregator) {
    return a.divide(b);
  }
}
