package org.enso.table.data.column.operation.map.numeric.arithmetic;

import java.math.BigDecimal;

import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.Storage;

public class BigDecimalDivideOp<T extends Number, I extends Storage<? super T>>
    extends NumericBinaryOpReturningBigDecimal<T, I> {
  public BigDecimalDivideOp() {
    super(Storage.Maps.DIV);
  }

  @Override
  public BigDecimal doBigDecimal(
      BigDecimal a, BigDecimal b, int ix, MapOperationProblemAggregator problemAggregator) {
    return a.divide(b);
  }
}
