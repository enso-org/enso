package org.enso.table.data.column.operation;

import java.util.function.BiFunction;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

public interface BinaryOperation {
  /**
   * Runs a 2-argument function on each element in the column.
   *
   * @param left the first column to run the function on.
   * @param right the second argument to pass to each run of the function on. If a column is passed,
   *     it will be zipped with the first column.
   * @param skipNulls specifies whether null values on the input should result in a null result
   *     without passing them through the function, this is useful if the function does not support
   *     the null-values, but it needs to be set to false if the function should handle them.
   * @param newName the name of the result column.
   * @param function the function to run.
   * @param expectedResultType the expected type for the result storage.
   * @param problemAggregator Problem aggregator to report problems to.
   * @return a new storage containing results of the function for each row
   */
  static Column mapFunction(
      Column left,
      Object right,
      Boolean skipNulls,
      String newName,
      BiFunction<Object, Object, Object> function,
      StorageType<?> expectedResultType,
      MapOperationProblemAggregator problemAggregator) {
    // Special handling for nulls
    if (skipNulls) {
      if (right instanceof Column rightColumn && rightColumn.getType() instanceof NullType) {
        right = null;
      }
      if (right == null
          || ColumnStorageWithInferredStorage.resolveStorage(left).getType() instanceof NullType) {
        var result =
            expectedResultType == null
                ? Builder.fromRepeatedItem(null, left.getSize())
                : expectedResultType
                    .makeBuilder(left.getSize(), problemAggregator)
                    .appendNulls(left.getSize())
                    .seal();
        return new Column(newName, result);
      }
    }

    ColumnStorage<?> result;
    if (right instanceof Column rightColumn) {
      result =
          StorageIterators.zipOverObjectStorages(
              left.getStorage(),
              rightColumn.getStorage(),
              size ->
                  expectedResultType == null
                      ? Builder.getInferredBuilder(size, problemAggregator)
                      : expectedResultType.makeBuilder(size, problemAggregator),
              skipNulls,
              (index, l, r) -> Polyglot_Utils.convertPolyglotValue(function.apply(l, r)));
    } else {
      var size = left.getSize();
      var builder =
          expectedResultType == null
              ? Builder.getInferredBuilder(size, problemAggregator)
              : expectedResultType.makeBuilder(size, problemAggregator);

      final var r = right;
      result =
          StorageIterators.buildObjectOverStorage(
              left.getStorage(),
              skipNulls,
              builder,
              (b, index, value) -> {
                var converted = Polyglot_Utils.convertPolyglotValue(function.apply(value, r));
                if (converted == null) {
                  b.appendNulls(1);
                } else {
                  b.append(converted);
                }
              });
    }
    return new Column(newName, result);
  }

  /*
   * Gets the storage of the column resolving through inferred storages.
   * Replace with a simple call to `getStorage` if an operation should not
   * resolve inferred storages.
   * */
  default ColumnStorage<?> getStorage(Column input) {
    return ColumnStorageWithInferredStorage.resolveStorage(input);
  }

  /**
   * Checks if the operation can be applied to the given columns.
   *
   * @param left the left column.
   * @param right the right column or value.
   * @return true if the operation can be applied, false otherwise.
   */
  default boolean canApply(Column left, Object right) {
    return right instanceof Column rightColumn
        ? canApplyZip(getStorage(left), getStorage(rightColumn))
        : canApplyMap(getStorage(left), right);
  }

  /**
   * Applies the operation to the given columns.
   *
   * @param left the left column.
   * @param right the right column or value.
   * @param newName the name of the new column.
   * @return the result of the operation.
   */
  default Column apply(
      Column left, Object right, String newName, MapOperationProblemAggregator problemAggregator) {
    ColumnStorage<?> leftStorage = getStorage(left);

    ColumnStorage<?> output;
    if (right instanceof Column rightColumn) {
      ColumnStorage<?> rightStorage = getStorage(rightColumn);
      if (!canApplyZip(leftStorage, rightStorage)) {
        throw new IllegalArgumentException("Cannot apply zip");
      }
      output = applyZip(leftStorage, rightStorage, problemAggregator);
    } else {
      if (!canApplyMap(leftStorage, right)) {
        throw new IllegalArgumentException("Cannot apply map");
      }
      output = applyMap(leftStorage, right, problemAggregator);
    }

    return new Column(newName, output);
  }

  /** Can the map be applied to the pair of ColumnStorage and constant? */
  boolean canApplyMap(ColumnStorage<?> left, Object rightValue);

  /** Can the map be applied to the pair of ColumnStorage? */
  boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right);

  /** Apply the map to the pair of ColumnStorage and constant. */
  ColumnStorage<?> applyMap(
      ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator);

  /** Apply the map to the pair of ColumnStorage. */
  ColumnStorage<?> applyZip(
      ColumnStorage<?> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator);
}
