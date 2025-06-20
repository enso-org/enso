package org.enso.table.data.column.operation;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;

public final class IfOperation {
  /**
   * The IfOperation class provides a way to apply a conditional operation on a column. This
   * verifies if the condition is valid for the operation.
   */
  private static boolean canApply(ColumnStorage<?> condition) {
    var conditionType = condition.getType();
    return conditionType instanceof BooleanType || conditionType instanceof NullType;
  }

  /**
   * Performs a conditional operation on a column.
   *
   * @param condition the condition column
   * @param new_name the name of the new column
   * @param when_true the value or column to return when the condition is true
   * @param when_false the value or column to return when the condition is false
   * @param resultStorageType the type of the result column
   * @param problemAggregator the problem aggregator to report problems to
   * @return the result column
   */
  public static <T> Column apply(
      Column condition,
      String new_name,
      Object when_true,
      Object when_false,
      StorageType<T> resultStorageType,
      ProblemAggregator problemAggregator) {
    // Check if the condition is valid
    var conditionStorage = ColumnStorageWithInferredStorage.resolveStorage(condition);
    if (!canApply(conditionStorage)) {
      throw new IllegalStateException(
          "Unsupported condition type: "
              + conditionStorage.getType()
              + ". This is a bug in the Table library.");
    }

    var result =
        (resultStorageType instanceof NullType)
            ? Builder.fromRepeatedItem(null, condition.getSize())
            : computeColumnStorage(
                condition,
                when_true,
                when_false,
                resultStorageType,
                problemAggregator,
                conditionStorage);
    return new Column(new_name, result);
  }

  private static <T> ColumnStorage<T> computeColumnStorage(
      Column condition,
      Object whenTrue,
      Object whenFalse,
      StorageType<T> resultStorageType,
      ProblemAggregator problemAggregator,
      ColumnStorage<?> conditionStorage) {
    var conditionType = conditionStorage.getType();

    // Handle set of Nulls for condition
    if (conditionType instanceof NullType) {
      return resultStorageType
          .makeBuilder(conditionStorage.getSize(), problemAggregator)
          .appendNulls(Builder.checkSize(conditionStorage.getSize()))
          .seal();
    }

    // Make step
    StorageIterators.BooleanBuildOperation<BuilderForType<T>> stepAction;
    if (whenTrue instanceof Column whenTrueColumn) {
      if (whenFalse instanceof Column whenFalseColumn) {
        // If both are columns, we can use the same index for both
        stepAction =
            (builder, index, value, isNothing) -> {
              builder.append(
                  value ? whenTrueColumn.getItem(index) : whenFalseColumn.getItem(index));
            };
      } else {
        // If only one is a column, we use it for true values and convert false to constant
        T whenFalseAsT = resultStorageType.valueAsType(whenFalse);
        stepAction =
            (builder, index, value, isNothing) -> {
              builder.append(value ? whenTrueColumn.getItem(index) : whenFalse);
            };
      }
    } else {
      T whenTrueAsT = resultStorageType.valueAsType(whenTrue);
      if (whenFalse instanceof Column whenFalseColumn) {
        // Just False is a column
        stepAction =
            (builder, index, value, isNothing) ->
                builder.append(value ? whenTrueAsT : whenFalseColumn.getItem(index));
      } else {
        // Otherwise, we convert the values to a constant row provider
        T whenFalseAsT = resultStorageType.valueAsType(whenFalse);
        stepAction =
            (builder, index, value, isNothing) ->
                builder.append(value ? whenTrueAsT : whenFalseAsT);
      }
    }

    return StorageIterators.buildOverBooleanStorage(
        BooleanType.INSTANCE.asTypedStorage(conditionStorage),
        resultStorageType.makeBuilder(condition.getSize(), problemAggregator),
        stepAction);
  }
}
