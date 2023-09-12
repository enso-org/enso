package org.enso.table.data.column.operation.map.numeric.arithmetic;

import java.math.BigInteger;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;

public interface NumericBinaryOpDefinition {
  double doDouble(double a, double b, int ix, MapOperationProblemBuilder problemBuilder);

  Long doLong(long a, long b, int ix, MapOperationProblemBuilder problemBuilder);

  BigInteger doBigInteger(
      BigInteger a, BigInteger b, int ix, MapOperationProblemBuilder problemBuilder);
}
