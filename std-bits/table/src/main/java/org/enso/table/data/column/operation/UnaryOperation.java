package org.enso.table.data.column.operation;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.LongConsumer;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;
import org.graalvm.polyglot.Context;
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
    ColumnStorage storage = column.getStorage();

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
    applyStorageInner(
        column.getStorage(),
        nothingUnchanged,
        storageBuilder,
        i -> {
          Value result = function.apply(column.getStorage().getItemAsObject(i));
          Object converted = Polyglot_Utils.convertPolyglotValue(result);
          storageBuilder.appendNoGrow(converted);
        });
    return new Column(newColumnName, storageBuilder.seal());
  }

  /** Gets the name of the Operation. */
  String getName();

  /** Can the operation be applied to the given Storage? */
  boolean canApply(ColumnStorage storage);

  /** Applies the operation to the given Storage. */
  ColumnStorage apply(ColumnStorage storage, MapOperationProblemAggregator problemAggregator);

  private static void applyStorageInner(
      ColumnStorage columnStorage,
      boolean nothingUnchanged,
      Builder builder,
      LongConsumer callback) {
    Context context = Context.getCurrent();
    long size = columnStorage.getSize();
    for (long i = 0; i < size; i++) {
      if (nothingUnchanged && columnStorage.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        callback.accept(i);
      }
      context.safepoint();
    }
  }

  /** Applies the operation to the given Storage. */
  static void applyOverObjectStorage(
      ColumnStorage objectStorage,
      boolean nothingUnchanged,
      Builder builder,
      Consumer<Object> function) {
    applyStorageInner(
        objectStorage,
        nothingUnchanged,
        builder,
        i -> function.accept(objectStorage.getItemAsObject(i)));
  }

  /** Applies the operation to the given Boolean Storage. */
  static void applyOverBooleanStorage(
      ColumnBooleanStorage booleanStorage,
      boolean nothingUnchanged,
      Builder builder,
      BooleanRowApplier function) {
    applyStorageInner(
        booleanStorage,
        nothingUnchanged,
        builder,
        i -> function.accept(booleanStorage.isNothing(i), booleanStorage.get(i)));
  }

  @FunctionalInterface
  interface BooleanRowApplier {
    void accept(boolean isNothing, boolean value);
  }

  /** Applies the operation to the given Long Storage. */
  static void applyOverLongStorage(
      ColumnLongStorage longStorage,
      boolean nothingUnchanged,
      Builder builder,
      LongRowApplier function) {
    applyStorageInner(
        longStorage,
        nothingUnchanged,
        builder,
        i -> function.accept(longStorage.isNothing(i), longStorage.get(i)));
  }

  @FunctionalInterface
  interface LongRowApplier {
    void accept(boolean isNothing, long value);
  }

  /** Applies the operation to the given Double Storage. */
  static void applyOverDoubleStorage(
      ColumnDoubleStorage doubleStorage,
      boolean nothingUnchanged,
      Builder builder,
      DoubleRowApplier function) {
    applyStorageInner(
        doubleStorage,
        nothingUnchanged,
        builder,
        i -> function.accept(doubleStorage.isNothing(i), doubleStorage.get(i)));
  }

  @FunctionalInterface
  interface DoubleRowApplier {
    void accept(boolean isNothing, double value);
  }
}
