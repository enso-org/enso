package org.enso.table.data.column.operation.map.numeric.arithmetic;

import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.Storage;

import java.math.BigDecimal;
import java.math.BigInteger;

public class MaxOp<T extends Number, I extends Storage<? super T>> extends NumericBinaryOpImplementation<T, I> {
  public MaxOp() {
    super(Storage.Maps.MAX);
  }

  @Override
  public double doDouble(double a, double b, int ix, MapOperationProblemAggregator problemAggregator) {
    return Math.max(a, b);
  }

  @Override
  public Long doLong(long a, long b, int ix, MapOperationProblemAggregator problemAggregator) {
    return Math.max(a, b);
  }

  @Override
  public BigInteger doBigInteger(BigInteger a, BigInteger b, int ix, MapOperationProblemAggregator problemAggregator) {
    return a.max(b);
  }

  @Override
  public BigDecimal doBigDecimal(BigDecimal a, BigDecimal b, int ix, MapOperationProblemAggregator problemAggregator) {
    return a.max(b);
  }
}
