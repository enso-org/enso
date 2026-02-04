package org.enso.table.data.column.operation.unary;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

public abstract class NumericUnaryOperation implements UnaryOperation {
  @Override
  public final boolean canApply(ColumnStorage<?> storage) {
    return switch (StorageType.ofStorage(storage)) {
      case IntegerType _, FloatType _, BigIntegerType _, BigDecimalType _, NullType _ -> true;
      default -> false;
    };
  }

  @Override
  public final ColumnStorage<?> apply(
      ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    var inputStorageType = StorageType.ofStorage(storage);
    return switch (inputStorageType) {
      case NullType _ -> Builder.fromRepeatedItem(null, storage.getSize());
      case FloatType ft ->
          StorageIterators.mapOverDoubleStorage(
              ft.asTypedStorage(storage),
              Builder.getForDouble(ft, storage.getSize(), problemAggregator),
              (index, value, isNothing) -> doDouble(value));
      case IntegerType it ->
          StorageIterators.mapOverLongStorage(
              it.asTypedStorage(storage),
              Builder.getForLong(it, storage.getSize(), problemAggregator),
              (index, value, isNothing) -> doLong(value));
      case BigIntegerType bigIntegerType ->
          StorageIterators.mapOverStorage(
              bigIntegerType.asTypedStorage(storage),
              Builder.getForBigInteger(storage.getSize(), problemAggregator),
              (index, value) -> doBigInteger(value));
      case BigDecimalType bigDecimalType ->
          StorageIterators.mapOverStorage(
              bigDecimalType.asTypedStorage(storage),
              Builder.getForBigDecimal(storage.getSize()),
              (index, value) -> doBigDecimal(value));
      default -> {
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
        yield builder.seal();
      }
    };
  }

  protected abstract double doDouble(double x);

  protected abstract long doLong(long x);

  protected abstract BigInteger doBigInteger(BigInteger x);

  protected abstract BigDecimal doBigDecimal(BigDecimal x);
}
