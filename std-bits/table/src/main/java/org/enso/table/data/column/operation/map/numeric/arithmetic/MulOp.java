package org.enso.table.data.column.operation.map.numeric.arithmetic;

import java.math.BigInteger;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.IntegerType;

public class MulOp<T extends Number, I extends Storage<? super T>>
    extends NumericBinaryOpImplementation<T, I> {
  public MulOp() {
    super(Storage.Maps.MUL);
  }

  @Override
  public double doDouble(
      double a, double b, int ix, MapOperationProblemAggregator problemAggregator) {
    return a * b;
  }

  @Override
  public Long doLong(long a, long b, int ix, MapOperationProblemAggregator problemAggregator) {
    try {
      return Math.multiplyExact(a, b);
    } catch (ArithmeticException e) {
      problemAggregator.reportOverflow(IntegerType.INT_64, a, "*", b);
      return null;
    }
  }

  @Override
  public BigInteger doBigInteger(
      BigInteger a, BigInteger b, int ix, MapOperationProblemAggregator problemAggregator) {
    return a.multiply(b);
  }
}
