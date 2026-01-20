package org.enso.table.data.column.operation.binary;

import static org.enso.table.data.column.operation.binary.BinaryOperator.createNumeric;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Random;
import org.enso.table.data.column.operation.BinaryOperationTyped;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

public class RandBetweenOperation extends BinaryOperator.NumericOperation {
  public static BinaryOperationTyped<?> create(Column left, Object right, long seed) {
    var rng = seed == 0 ? new Random() : new Random(seed);
    var leftStorage = ColumnStorageWithInferredStorage.resolveStorage(left);
    return createNumeric(leftStorage.getType(), right, new RandBetweenOperation(rng));
  }

  private final Random rng;

  RandBetweenOperation(Random rng) {
    this.rng = rng;
  }

  @Override
  BigDecimal doBigDecimal(
      BigDecimal a, BigDecimal b, long ix, MapOperationProblemAggregator problemAggregator) {
    double distance = rng.nextDouble();
    var distDecimal = b.subtract(a);
    var scaled = new BigDecimal(distance).multiply(distDecimal);
    return a.add(scaled);
  }

  @Override
  Double doDouble(double a, double b, long ix, MapOperationProblemAggregator problemAggregator) {
    return rng.nextDouble() * (b - a) + a;
  }

  @Override
  Long doLong(long a, long b, long ix, MapOperationProblemAggregator problemAggregator) {
    return rng.nextLong(a, b);
  }

  @Override
  BigInteger doBigInteger(
      BigInteger a, BigInteger b, long ix, MapOperationProblemAggregator problemAggregator) {
    double distance = rng.nextDouble();
    var distInteger = b.subtract(a);
    var scaled = new BigDecimal(distance).multiply(new BigDecimal(distInteger));
    return a.add(scaled.toBigInteger());
  }
}
