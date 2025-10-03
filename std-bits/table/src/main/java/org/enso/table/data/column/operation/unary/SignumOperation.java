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

public final class SignumOperation implements UnaryOperation {
  public static final UnaryOperation INSTANCE = new SignumOperation();

  private SignumOperation() {}

  @Override
  public String getName() {
    return "signum";
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
      return StorageIterators.mapOverLongStorage(
          columnLongStorage,
          Builder.getForLong(IntegerType.INT_8, storage.getSize(), problemAggregator),
          (index, value, isNothing) -> signum(value));
    }

    if (storage instanceof ColumnDoubleStorage columnStorage) {
      return StorageIterators.mapOverStorage(
          columnStorage,
          Builder.getForDouble(FloatType.FLOAT_64, storage.getSize(), problemAggregator),
          (index, value) -> signum(value));
    }

    if (storage.getType() instanceof BigIntegerType bigIntegerType) {
      ColumnStorage<BigInteger> bigIntegerColumnStorage = bigIntegerType.asTypedStorage(storage);
      return StorageIterators.mapOverStorage(
          bigIntegerColumnStorage,
          Builder.getForLong(IntegerType.INT_8, storage.getSize(), problemAggregator),
          (index, value) -> signum(value));
    }

    if (storage.getType() instanceof BigDecimalType bigDecimalType) {
      ColumnStorage<BigDecimal> bigDecimalColumnStorage = bigDecimalType.asTypedStorage(storage);
      return StorageIterators.mapOverStorage(
          bigDecimalColumnStorage,
          Builder.getForLong(IntegerType.INT_8, storage.getSize(), problemAggregator),
          (index, value) -> signum(value));
    }

    // Fallback for Mixed and any other storage implementations
    var builder = Builder.getInferredBuilder(storage.getSize(), problemAggregator);
    for (long i = 0; i < storage.getSize(); i++) {
      if (storage.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        var item = storage.getItemBoxed(i);
        switch (item) {
          case Long lng -> builder.append(signum(lng));
          case Double dbl -> builder.append(signum(dbl));
          case BigInteger bi -> builder.append(signum(bi));
          case BigDecimal bd -> builder.append(signum(bd));
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

  private long signum(long x) {
    return Long.signum(x);
  }

  private double signum(double x) {
    return Math.signum(x);
  }

  private long signum(BigInteger x) {
    return x.signum();
  }

  private long signum(BigDecimal x) {
    return x.signum();
  }
}
