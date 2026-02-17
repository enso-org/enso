package org.enso.table.data.column.operation.unary;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
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
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

/**
 * A base class for numeric unary operations that operate on various numeric types such as integers,
 * floats, big integers, and big decimals returning a unified numeric type (e.g. Long or Double).
 */
public abstract class NumericUnaryTypedOperation<T> implements UnaryOperation {
  protected final StorageType<T> storageType;
  protected final String name;

  protected NumericUnaryTypedOperation(String name, StorageType<T> storageType) {
    this.name = name;
    this.storageType = storageType;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public boolean canApply(ColumnStorage<?> storage) {
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
  public ColumnStorage<?> apply(
      ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    if (storage.getType() instanceof NullType) {
      return Builder.fromRepeatedItem(null, storage.getSize());
    }

    if (storage instanceof ColumnLongStorage columnLongStorage) {
      return StorageIterators.buildOverLongStorage(
          columnLongStorage,
          storageType.makeBuilder(storage.getSize(), problemAggregator),
          (builder, index, value, isNothing) -> doLong(builder, value));
    }

    if (storage instanceof ColumnDoubleStorage columnDoubleStorage) {
      return StorageIterators.buildOverDoubleStorage(
          columnDoubleStorage,
          storageType.makeBuilder(storage.getSize(), problemAggregator),
          (builder, index, value, isNothing) -> doDouble(builder, value));
    }

    if (storage.getType() instanceof BigIntegerType bigIntegerType) {
      ColumnStorage<BigInteger> bigIntegerColumnStorage = bigIntegerType.asTypedStorage(storage);
      return StorageIterators.buildOverStorage(
          bigIntegerColumnStorage,
          storageType.makeBuilder(storage.getSize(), problemAggregator),
          (builder, index, value) -> doBigInteger(builder, value));
    }

    if (storage.getType() instanceof BigDecimalType bigDecimalType) {
      ColumnStorage<BigDecimal> bigDecimalColumnStorage = bigDecimalType.asTypedStorage(storage);
      return StorageIterators.buildOverStorage(
          bigDecimalColumnStorage,
          storageType.makeBuilder(storage.getSize(), problemAggregator),
          (builder, index, value) -> doBigDecimal(builder, value));
    }

    // Fallback for Mixed and any other storage implementations
    var builder = storageType.makeBuilder(storage.getSize(), problemAggregator);
    for (long i = 0; i < storage.getSize(); i++) {
      if (storage.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        var item = storage.getItemBoxed(i);
        switch (item) {
          case Long lng -> doLong(builder, lng);
          case Double dbl -> doDouble(builder, dbl);
          case BigInteger bi -> doBigInteger(builder, bi);
          case BigDecimal bd -> doBigDecimal(builder, bd);
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

  protected abstract void doLong(BuilderForType<T> builder, long x);

  protected abstract void doDouble(BuilderForType<T> builder, double x);

  protected abstract void doBigInteger(BuilderForType<T> builder, BigInteger x);

  protected abstract void doBigDecimal(BuilderForType<T> builder, BigDecimal x);
}
