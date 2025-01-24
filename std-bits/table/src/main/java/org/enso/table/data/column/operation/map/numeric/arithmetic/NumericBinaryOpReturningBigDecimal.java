package org.enso.table.data.column.operation.map.numeric.arithmetic;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.numeric.helpers.BigDecimalArrayAdapter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;

public abstract class NumericBinaryOpReturningBigDecimal<
        T extends Number, I extends Storage<? super T>>
    extends NumericBinaryOpImplementation<T, I> {
  public NumericBinaryOpReturningBigDecimal(String name) {
    super(name);
  }

  @Override
  public Storage<? extends Number> runBinaryMap(
      I storage, Object arg, MapOperationProblemAggregator problemAggregator) {
    if (arg == null) {
      return BigDecimalStorage.makeEmpty(storage.getSize());
    }

    BigDecimalArrayAdapter lhs = BigDecimalArrayAdapter.fromAnyStorage(storage);
    BigDecimal rhs = NumericConverter.coerceToBigDecimal(arg);
    return runBigDecimalMap(lhs, rhs, problemAggregator);
  }

  @Override
  public Storage<? extends Number> runZip(
      I storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
    BigDecimalArrayAdapter left = BigDecimalArrayAdapter.fromAnyStorage(storage);
    BigDecimalArrayAdapter right = BigDecimalArrayAdapter.fromAnyStorage(arg);
    return runBigDecimalZip(left, right, problemAggregator);
  }

  @Override
  public Long doLong(long a, long b, long ix, MapOperationProblemAggregator problemAggregator) {
    throw new IllegalStateException(
        "Impossible: should not reach here - a NumericOpReturningBigDecimal should always use the"
            + " doBigDecimal branch.");
  }

  @Override
  public BigInteger doBigInteger(
      BigInteger a, BigInteger b, long ix, MapOperationProblemAggregator problemAggregator) {
    throw new IllegalStateException(
        "Impossible: should not reach here - a NumericOpReturningBigDecimal should always use the"
            + " doBigDecimal branch.");
  }

  @Override
  public double doDouble(
      double a, double b, long ix, MapOperationProblemAggregator problemAggregator) {
    throw new IllegalStateException(
        "Impossible: should not reach here - a NumericOpReturningBigDecimal should always use the"
            + " doBigDecimal branch.");
  }
}
