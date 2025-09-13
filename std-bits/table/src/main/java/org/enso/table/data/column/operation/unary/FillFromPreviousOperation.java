package org.enso.table.data.column.operation.unary;

import java.util.Objects;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

public class FillFromPreviousOperation implements UnaryOperation {
  public static final FillFromPreviousOperation INSTANCE = new FillFromPreviousOperation(false);

  public static final FillFromPreviousOperation FILL_EMPTY = new FillFromPreviousOperation(true);

  private final boolean fillEmpty;

  private FillFromPreviousOperation(boolean fillEmpty) {
    this.fillEmpty = fillEmpty;
  }

  @Override
  public String getName() {
    return "fill_from_previous";
  }

  @Override
  public boolean canApply(ColumnStorage<?> storage) {
    return true;
  }

  private static class ObjectState {
    public Object prevValue = null;
  }

  private static class BooleanState {
    public boolean isNothing = true;
    public boolean prevValue = false;
  }

  private static class DoubleState {
    public boolean isNothing = true;
    public double prevValue = 0.0;
  }

  private static class LongState {
    public boolean isNothing = true;
    public long prevValue = 0;
  }

  @Override
  public ColumnStorage<?> apply(
      ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    if (storage.getType() instanceof NullType) {
      return storage; // Nothing to fill in a column of nulls.
    }
    return switch (storage) {
      case ColumnBooleanStorage boolStorage -> {
        var state = new BooleanState();
        yield StorageIterators.buildOverBooleanStorage(
            boolStorage,
            false,
            Builder.getForBoolean(boolStorage.getSize()),
            (builder, idx, value, isNothing) -> {
              if (!isNothing) {
                state.isNothing = false;
                state.prevValue = value;
              }
              if (state.isNothing) {
                builder.appendNulls(1);
              } else {
                builder.appendBoolean(state.prevValue);
              }
            });
      }
      case ColumnDoubleStorage doubleStorage -> {
        var state = new DoubleState();
        yield StorageIterators.buildOverDoubleStorage(
            doubleStorage,
            false,
            Builder.getForDouble(
                doubleStorage.getType(), doubleStorage.getSize(), problemAggregator),
            (builder, idx, value, isNothing) -> {
              if (!isNothing) {
                state.isNothing = false;
                state.prevValue = value;
              }
              if (state.isNothing) {
                builder.appendNulls(1);
              } else {
                builder.appendDouble(state.prevValue);
              }
            });
      }
      case ColumnLongStorage longStorage -> {
        var state = new LongState();
        yield StorageIterators.buildOverLongStorage(
            longStorage,
            false,
            Builder.getForLong(longStorage.getType(), longStorage.getSize(), problemAggregator),
            (builder, idx, value, isNothing) -> {
              if (!isNothing) {
                state.isNothing = false;
                state.prevValue = value;
              }
              if (state.isNothing) {
                builder.appendNulls(1);
              } else {
                builder.appendLong(state.prevValue);
              }
            });
      }
      default -> {
        var state = new ObjectState();
        yield StorageIterators.buildObjectOverStorage(
            storage,
            false,
            Builder.getForType(storage.getType(), storage.getSize(), problemAggregator),
            (builder, idx, value) -> {
              if (value != null && (!fillEmpty || !Objects.equals("", value))) {
                state.prevValue = value;
              }
              builder.append(state.prevValue == null ? value : state.prevValue);
            });
      }
    };
  }
}
