package org.enso.table.data.column.operation.map.numeric.arithmetic;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.Storage;

public class ModOp<T extends Number, I extends Storage<? super T>>
    extends NumericBinaryOpImplementation<T, I> {
  public ModOp() {
    super(Storage.Maps.MOD);
  }

  @Override
  public double doDouble(
      double a, double b, long ix, MapOperationProblemAggregator problemAggregator) {
    if (b == 0.0) {
      // ToDo: ProblemAggregator should accept a long instead of an int.
      problemAggregator.reportDivisionByZero((int) ix);
    }

    return a % b;
  }

  @Override
  public Long doLong(long a, long b, long ix, MapOperationProblemAggregator problemAggregator) {
    if (b == 0) {
      // ToDo: ProblemAggregator should accept a long instead of an int.
      problemAggregator.reportDivisionByZero((int) ix);
      return null;
    }

    return a % b;
  }

  @Override
  public BigInteger doBigInteger(
      BigInteger a, BigInteger b, long ix, MapOperationProblemAggregator problemAggregator) {
    if (b.equals(BigInteger.ZERO)) {
      // ToDo: ProblemAggregator should accept a long instead of an int.
      problemAggregator.reportDivisionByZero((int) ix);
      return null;
    }

    return a.mod(b);
  }

  @Override
  public BigDecimal doBigDecimal(
      BigDecimal a, BigDecimal b, long ix, MapOperationProblemAggregator problemAggregator) {
    if (b.equals(BigDecimal.ZERO)) {
      // ToDo: ProblemAggregator should accept a long instead of an int.
      problemAggregator.reportDivisionByZero((int) ix);
      return null;
    }

    return a.remainder(b);
  }
}
