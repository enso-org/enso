package org.enso.table.data.column.operation.map.numeric.arithmetic;

import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;

import java.math.BigInteger;

public interface NumericBinaryOpDefinition {
  double doDouble(double a, double b, int ix, MapOperationProblemBuilder problemBuilder);

  Long doLong(long a, long b, int ix, MapOperationProblemBuilder problemBuilder);

  BigInteger doBigInteger(BigInteger a, BigInteger b, int ix, MapOperationProblemBuilder problemBuilder);
}
