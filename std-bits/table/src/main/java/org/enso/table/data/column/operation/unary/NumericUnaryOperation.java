package org.enso.table.data.column.operation.unary;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

public abstract class NumericUnaryOperation implements UnaryOperation {

  @Override
  public final boolean canApply(ColumnStorage<?> storage) {
    return switch (storage.getType()) {
      case IntegerType ignored -> true;
      case FloatType ignored -> true;
      case BigIntegerType ignored -> true;
      case BigDecimalType ignored -> true;
      case NullType ignored -> true;
      default -> false;
    };
  }

  @Override
  public final ColumnStorage<?> apply(
      ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    if (storage.getType() instanceof NullType) {
      return Builder.fromRepeatedItem(null, storage.getSize());
    }

    if (storage instanceof ColumnDoubleStorage columnDoubleStorage) {
      return StorageIterators.mapOverDoubleStorage(
          columnDoubleStorage,
          Builder.getForDouble(
              columnDoubleStorage.getType(), columnDoubleStorage.getSize(), problemAggregator),
          (index, value, isNothing) -> doDouble(value));
    }

    if (storage instanceof ColumnLongStorage columnLongStorage) {
      return StorageIterators.mapOverLongStorage(
          columnLongStorage,
          Builder.getForLong(
              columnLongStorage.getType(), columnLongStorage.getSize(), problemAggregator),
          (index, value, isNothing) -> doLong(value));
    }

    if (storage.getType() instanceof BigIntegerType bigIntegerType) {
      ColumnStorage<BigInteger> bigIntegerColumnStorage = bigIntegerType.asTypedStorage(storage);
      return StorageIterators.mapOverStorage(
          bigIntegerColumnStorage,
          Builder.getForBigInteger(storage.getSize(), problemAggregator),
          (index, value) -> doBigInteger(value));
    }

    if (storage.getType() instanceof BigDecimalType bigDecimalType) {
      ColumnStorage<BigDecimal> bigDecimalColumnStorage = bigDecimalType.asTypedStorage(storage);
      return StorageIterators.mapOverStorage(
          bigDecimalColumnStorage,
          Builder.getForBigDecimal(storage.getSize()),
          (index, value) -> doBigDecimal(value));
    }

    // Fallback for Mixed and any other storage implementations
    var builder = Builder.getInferredBuilder(storage.getSize(), problemAggregator);
    for (long i = 0; i < storage.getSize(); i++) {
      if (storage.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        var item = storage.getItemBoxed(i);
        switch (item) {
          case Long lng -> builder.append(doLong(lng));
          case Double dbl -> builder.append(doDouble(dbl));
          case BigInteger bi -> builder.append(doBigInteger(bi));
          case BigDecimal bd -> builder.append(doBigDecimal(bd));
          default -> {
            builder.appendNulls(1);
            problemAggregator.reportIllegalArgumentError(
                "Unsupported type for signum operation: " + item, i);
          }
        }
      }
    }
    return builder.seal();
  }

  protected abstract double doDouble(double x);

  protected abstract long doLong(long x);

  protected abstract BigInteger doBigInteger(BigInteger x);

  protected abstract BigDecimal doBigDecimal(BigDecimal x);
}
