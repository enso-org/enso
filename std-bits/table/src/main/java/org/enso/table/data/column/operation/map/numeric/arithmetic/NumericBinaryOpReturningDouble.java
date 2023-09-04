package org.enso.table.data.column.operation.map.numeric.arithmetic;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.operation.map.numeric.helpers.DoubleArrayAdapter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;

import java.math.BigInteger;

import static org.enso.table.data.column.operation.map.numeric.helpers.DoubleArrayAdapter.fromAnyStorage;

public abstract class NumericBinaryOpReturningDouble<T extends Number, I extends Storage<? super T>> extends NumericBinaryOpImplementation<T, I> {
  public NumericBinaryOpReturningDouble(String name) {
    super(name);
  }

  @Override
  public Storage<? extends Number> runBinaryMap(I storage, Object arg, MapOperationProblemBuilder problemBuilder) {
    if (arg == null) {
      return DoubleStorage.makeEmpty(storage.size());
    }

    DoubleArrayAdapter lhs = fromAnyStorage(storage);
    double rhs = (arg instanceof BigInteger bigInteger) ? bigInteger.doubleValue() :
        NumericConverter.coerceToDouble(arg);
    return runDoubleMap(lhs, rhs, problemBuilder);
  }

  @Override
  public Storage<? extends Number> runZip(I storage, Storage<?> arg, MapOperationProblemBuilder problemBuilder) {
    DoubleArrayAdapter lhs = fromAnyStorage(storage);
    DoubleArrayAdapter rhs = fromAnyStorage(arg);
    return runDoubleZip(lhs, rhs, problemBuilder);
  }

  @Override
  public Long doLong(long a, long b, int ix, MapOperationProblemBuilder problemBuilder) {
    throw new IllegalStateException("Impossible: should not reach here - a NumericOpReturningDouble should always use the doDouble branch.");
  }

  @Override
  public BigInteger doBigInteger(BigInteger a, BigInteger b, int ix, MapOperationProblemBuilder problemBuilder) {
    throw new IllegalStateException("Impossible: should not reach here - a NumericOpReturningDouble should always use the doDouble branch.");
  }
}
