package org.enso.table.data.column.operation;

import java.util.function.Function;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;
import org.graalvm.polyglot.Value;

/** A UnaryOperation is an operation that can be applied to a single ColumnStorage. */
public interface UnaryOperation {
  /**
   * Applies the operation to the given Column. If an unsupported by the operation returns null
   *
   * @param column the column to apply the operation to.
   * @param operation the operation to apply.
   * @param newColumnName the name of the new column.
   * @param problemAggregator the problem aggregator to report problems to.
   */
  static Column apply(
      Column column,
      UnaryOperation operation,
      String newColumnName,
      MapOperationProblemAggregator problemAggregator) {
    ColumnStorage<?> storage = column.getStorage();

    // If the storage has an inferred storage (e.g. a Mixed column) and the first level can't do get
    // an inferred storage.
    if (!operation.canApply(storage)
        && storage instanceof ColumnStorageWithInferredStorage withInferredStorage) {
      var inferredStorage = withInferredStorage.getInferredStorage();
      if (inferredStorage != null && operation.canApply(inferredStorage)) {
        storage = inferredStorage;
      }
    }

    if (!operation.canApply(storage)) {
      return null;
    }

    var result = operation.apply(column.getStorage(), problemAggregator);
    return new Column(newColumnName, (Storage<?>) result);
  }

  /**
   * Applies a function to every row in a column.
   *
   * @param column the column to apply the operation to.
   * @param function the function to apply.
   * @param nothingUnchanged whether to keep nothing values unchanged.
   * @param expectedResultType the expected type of the result.
   * @param newColumnName the name of the new column.
   * @param problemAggregator the problem aggregator to report problems to.
   */
  static Column mapFunction(
      Column column,
      Function<Object, Value> function,
      boolean nothingUnchanged,
      StorageType expectedResultType,
      String newColumnName,
      MapOperationProblemAggregator problemAggregator) {
    Builder storageBuilder =
        Builder.getForType(expectedResultType, column.getSize(), problemAggregator);

    var storage =
        StorageIterators.buildObjectOverStorage(
            column.getStorage(),
            nothingUnchanged,
            storageBuilder,
            (builder, index, value) -> {
              Value result = function.apply(value);
              Object converted = Polyglot_Utils.convertPolyglotValue(result);
              builder.append(converted);
            });

    return new Column(newColumnName, (Storage<?>) storage);
  }

  /** Gets the name of the Operation. */
  String getName();

  /** Can the operation be applied to the given Storage? */
  boolean canApply(ColumnStorage<?> storage);

  /** Applies the operation to the given Storage. */
  ColumnStorage<?> apply(ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator);
}
