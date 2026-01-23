package org.enso.table.data.column.operation.unary;

import java.util.Objects;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
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
    var storageType = StorageType.ofStorage(storage);
    return switch (storageType) {
      case NullType _ -> storage; // Nothing to fill in a column of nulls.
      case BooleanType bt -> {
        var boolStorage = bt.asTypedStorage(storage);
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
      case FloatType ft -> {
        var doubleStorage = ft.asTypedStorage(storage);
        var state = new DoubleState();
        yield StorageIterators.buildOverDoubleStorage(
            doubleStorage,
            false,
            Builder.getForDouble(ft, doubleStorage.getSize(), problemAggregator),
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
      case IntegerType it -> {
        var longStorage = it.asTypedStorage(storage);
        var state = new LongState();
        yield StorageIterators.buildOverLongStorage(
            longStorage,
            false,
            Builder.getForLong(it, longStorage.getSize(), problemAggregator),
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
            Builder.getForType(storageType, storage.getSize(), problemAggregator),
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
