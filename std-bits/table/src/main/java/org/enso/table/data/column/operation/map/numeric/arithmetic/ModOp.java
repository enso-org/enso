package org.enso.table.data.column.operation.map.numeric.arithmetic;

import java.math.BigInteger;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.storage.Storage;

public class ModOp<T extends Number, I extends Storage<? super T>>
    extends NumericBinaryOpImplementation<T, I> {
  public ModOp() {
    super(Storage.Maps.MOD);
  }

  @Override
  public double doDouble(double a, double b, int ix, MapOperationProblemBuilder problemBuilder) {
    if (b == 0.0) {
      problemBuilder.reportDivisionByZero(ix);
    }

    return a % b;
  }

  @Override
  public Long doLong(long a, long b, int ix, MapOperationProblemBuilder problemBuilder) {
    if (b == 0) {
      problemBuilder.reportDivisionByZero(ix);
      return null;
    }

    return a % b;
  }

  @Override
  public BigInteger doBigInteger(
      BigInteger a, BigInteger b, int ix, MapOperationProblemBuilder problemBuilder) {
    if (b.equals(BigInteger.ZERO)) {
      problemBuilder.reportDivisionByZero(ix);
      return null;
    }

    return a.mod(b);
  }
}
