package org.enso.table.data.column.operation.map.numeric.arithmetic;

import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.storage.Storage;

public class DivideOp<T extends Number, I extends Storage<? super T>>
    extends NumericBinaryOpReturningDouble<T, I> {
  public DivideOp() {
    super(Storage.Maps.DIV);
  }

  @Override
  public double doDouble(double a, double b, int ix, MapOperationProblemBuilder problemBuilder) {
    if (b == 0.0) {
      problemBuilder.reportDivisionByZero(ix);
    }
    return a / b;
  }
}
