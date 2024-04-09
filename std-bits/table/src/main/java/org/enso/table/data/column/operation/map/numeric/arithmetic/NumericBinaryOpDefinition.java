package org.enso.table.data.column.operation.map.numeric.arithmetic;

import java.math.BigInteger;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;

public interface NumericBinaryOpDefinition {
  double doDouble(double a, double b, int ix, MapOperationProblemAggregator problemAggregator);

  Long doLong(long a, long b, int ix, MapOperationProblemAggregator problemAggregator);

  BigInteger doBigInteger(
      BigInteger a, BigInteger b, int ix, MapOperationProblemAggregator problemAggregator);
}
