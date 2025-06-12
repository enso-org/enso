package org.enso.table.data.column.operation;

import java.util.function.LongFunction;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Value;

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
      Value when_true,
      Value when_false,
      StorageType<T> resultStorageType,
      ProblemAggregator problemAggregator) {
    // Check if the condition is valid
    var conditionStorage = BinaryOperation.getInferredStorage(condition);
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
      Value when_true,
      Value when_false,
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

    var on_true = makeRowProvider(when_true);
    var on_false = makeRowProvider(when_false);
    return StorageIterators.buildOverBooleanStorage(
        BooleanType.INSTANCE.asTypedStorage(conditionStorage),
        resultStorageType.makeBuilder(condition.getSize(), problemAggregator),
        (builder, index, value, isNothing) -> {
          builder.append(value ? on_true.apply(index) : on_false.apply(index));
        });
  }

  private static LongFunction<Object> makeRowProvider(Value value) {
    if (value.isHostObject() && value.asHostObject() instanceof Column column) {
      var storage = column.getStorage();
      return i -> (Object) storage.getItemBoxed(i);
    }
    var converted = Polyglot_Utils.convertPolyglotValue(value);
    return i -> converted;
  }
}
