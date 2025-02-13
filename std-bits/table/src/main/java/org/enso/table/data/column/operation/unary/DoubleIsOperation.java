package org.enso.table.data.column.operation.unary;

import java.util.BitSet;
import java.util.function.DoublePredicate;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithNothingMap;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;

public class DoubleIsOperation implements UnaryOperation {
  public static final String FINITE_NAME = "is_finite";
  public static final UnaryOperation IS_FINITE =
      new DoubleIsOperation(FINITE_NAME, Double::isFinite, true);

  public static final String INFINITE_NAME = "is_infinite";
  public static final UnaryOperation IS_INFINITE =
      new DoubleIsOperation(INFINITE_NAME, Double::isInfinite, false);

  public static final String NAN_NAME = "is_nan";
  public static final UnaryOperation IS_NAN = new DoubleIsOperation(NAN_NAME, Double::isNaN, false);

  private final String name;
  private final DoublePredicate predicate;
  private final boolean finiteValue;

  private DoubleIsOperation(String name, DoublePredicate predicate, boolean finiteValue) {
    this.name = name;
    this.predicate = predicate;
    this.finiteValue = finiteValue;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public boolean canApply(ColumnStorage<?> storage) {
    return storage.getType().isNumeric();
  }

  private static boolean isAllFinite(StorageType storageType) {
    return switch (storageType) {
      case IntegerType ignored -> true;
      case BigDecimalType ignored -> true;
      case BigIntegerType ignored -> true;
      default -> false;
    };
  }

  @Override
  public ColumnStorage<?> apply(
      ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    // For Finite
    if (isAllFinite(storage.getType())) {
      if (storage instanceof ColumnStorageWithNothingMap withNothingMap) {
        return new BoolStorage(
            new BitSet(), withNothingMap.getIsNothingMap(), (int) storage.getSize(), finiteValue);
      }

      return StorageIterators.mapOverStorage(
          storage, Builder.getForBoolean(storage.getSize()), (index, value) -> finiteValue);
    }

    // Avoid boxing here by using the specific storage types.
    if (storage instanceof ColumnDoubleStorage doubleStorage) {
      return StorageIterators.buildOverDoubleStorage(
          doubleStorage,
          Builder.getForBoolean(storage.getSize()),
          (builder, index, value, isNothing) -> builder.appendBoolean(predicate.test(value)));
    }

    return StorageIterators.mapOverStorage(
        storage,
        Builder.getForBoolean(storage.getSize()),
        (index, value) ->
            switch (value) {
              case Double d -> predicate.test(d);
              case Float f -> predicate.test(f);
              default -> false;
            });
  }
}
