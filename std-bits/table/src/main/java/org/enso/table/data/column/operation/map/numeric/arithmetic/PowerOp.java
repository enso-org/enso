package org.enso.table.data.column.operation.map.numeric.arithmetic;

import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.Storage;

public class PowerOp<T extends Number, I extends Storage<? super T>>
    extends NumericBinaryOpReturningDouble<T, I> {
  public PowerOp() {
    super(Storage.Maps.POWER);
  }

  @Override
  public double doDouble(
      double a, double b, int ix, MapOperationProblemAggregator problemAggregator) {
    return Math.pow(a, b);
  }
}
