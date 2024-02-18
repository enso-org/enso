package org.enso.table.data.column.operation;

import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.unary.*;
import org.enso.table.data.column.storage.*;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.LongConsumer;

/**
 * A UnaryOperation is an operation that can be applied to a single ColumnStorage.
 */
public interface UnaryOperation {
  final String TRUNCATE = "truncate";
  final String FLOOR = "floor";
  final String CEIL = "ceil";
  final String TEXT_LENGTH = "text_length";
  final String IS_NAN = "is_nan";
  final String IS_EMPTY = "is_empty";
  final String IS_INFINITE = "is_infinite";
  final String IS_NOTHING = "is_nothing";
  final String NOT = "not";
  final String YEAR = "year";
  final String QUARTER = "quarter";
  final String MONTH = "month";
  final String WEEK = "week";
  final String DAY = "day";
  final String HOUR = "hour";
  final String MINUTE = "minute";
  final String SECOND = "second";
  final String MILLISECOND = "millisecond";
  final String MICROSECOND = "microsecond";
  final String NANOSECOND = "nanosecond";

  /** Applies the operation to the given Column. If an unsupported by the operation returns null */
  static Column apply(Column column, String operationName, String newColumnName, ProblemAggregator problemAggregator) {
    UnaryOperation operation = UnaryOperation.getInstance(operationName);
    if (!operation.canApply(column.getStorage())) {
      return null;
    }

    var result = operation.apply(column.getStorage(), problemAggregator);
    return new Column(newColumnName, (Storage<?>) result);
  }

  static Column mapFunction(Column column, Function<Object, Value> function, boolean nothingUnchanged, StorageType expectedResultType, String newColumnName, ProblemAggregator problemAggregator) {
    Builder storageBuilder = Builder.getForType(expectedResultType, column.getSize(), problemAggregator);
    applyStorageInner(column.getStorage(), nothingUnchanged, storageBuilder, i -> {
      Value result = function.apply(column.getStorage().getItemAsObject(i));
      Object converted = Polyglot_Utils.convertPolyglotValue(result);
      storageBuilder.appendNoGrow(converted);
    });
    return new Column(newColumnName, storageBuilder.seal());
  }

  /** Gets the Operation with the given name. */
  static UnaryOperation getInstance(String name) {
    return switch (name) {
      case IS_NOTHING -> IsNothingOperation.INSTANCE;
      case NOT -> NotOperation.INSTANCE;
      case IS_EMPTY -> IsEmptyOperation.INSTANCE;
      case TEXT_LENGTH -> TextLengthOperation.INSTANCE;
      case IS_NAN -> IsNaNOperation.INSTANCE;
      case IS_INFINITE -> IsInfiniteOperation.INSTANCE;
      case YEAR -> DatePartOperation.YEAR_INSTANCE;
      case QUARTER -> DatePartOperation.QUARTER_INSTANCE;
      case MONTH -> DatePartOperation.MONTH_INSTANCE;
      case WEEK -> DatePartOperation.WEEK_INSTANCE;
      case DAY -> DatePartOperation.DAY_INSTANCE;
      case HOUR -> DatePartOperation.HOUR_INSTANCE;
      case MINUTE -> DatePartOperation.MINUTE_INSTANCE;
      case SECOND -> DatePartOperation.SECOND_INSTANCE;
      case MILLISECOND -> DatePartOperation.MILLISECOND_INSTANCE;
      case MICROSECOND -> DatePartOperation.MICROSECOND_INSTANCE;
      case NANOSECOND -> DatePartOperation.NANOSECOND_INSTANCE;
      default -> throw new IllegalArgumentException("Unknown unary operation: " + name + ".");
    };
  }

  /** Gets the name of the Operation. */
  String getName();

  /** Can the operation be applied to the given Storage? */
  boolean canApply(ColumnStorage storage);

  /** Applies the operation to the given Storage. */
  ColumnStorage apply(ColumnStorage storage, ProblemAggregator problemAggregator);

  private static void applyStorageInner(ColumnStorage columnStorage, boolean nothingUnchanged, Builder builder, LongConsumer callback) {
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
  static void applyOverObjectStorage(ColumnStorage objectStorage, boolean nothingUnchanged, Builder builder, Consumer<Object> function)
  {
    applyStorageInner(objectStorage, nothingUnchanged, builder, i -> function.accept(objectStorage.getItemAsObject(i)));
  }

  /** Applies the operation to the given Boolean Storage. */
  static void applyOverBooleanStorage(ColumnBooleanStorage booleanStorage, boolean nothingUnchanged, Builder builder, BooleanRowApplier function)
  {
    applyStorageInner(booleanStorage, nothingUnchanged, builder, i -> function.accept(booleanStorage.isNothing(i), booleanStorage.get(i)));
  }

  @FunctionalInterface
  interface BooleanRowApplier {
    void accept(boolean isNothing, boolean value);
  }

  /** Applies the operation to the given Long Storage. */
  static void applyOverLongStorage(ColumnLongStorage longStorage, boolean nothingUnchanged, Builder builder, LongRowApplier function)
  {
    applyStorageInner(longStorage, nothingUnchanged, builder, i -> function.accept(longStorage.isNothing(i), longStorage.get(i)));
  }

  @FunctionalInterface
  interface LongRowApplier {
    void accept(boolean isNothing, long value);
  }

  /** Applies the operation to the given Double Storage. */
  static void applyOverDoubleStorage(ColumnDoubleStorage doubleStorage, boolean nothingUnchanged, Builder builder, DoubleRowApplier function)
  {
    applyStorageInner(doubleStorage, nothingUnchanged, builder, i -> function.accept(doubleStorage.isNothing(i), doubleStorage.get(i)));
  }

  @FunctionalInterface
  interface DoubleRowApplier {
    void accept(boolean isNothing, double value);
  }
}
