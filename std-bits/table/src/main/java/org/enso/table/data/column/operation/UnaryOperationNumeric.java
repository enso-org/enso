package org.enso.table.data.column.operation;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;
import org.enso.table.error.UnexpectedTypeException;

public abstract class UnaryOperationNumeric<T, R> implements UnaryOperation {
  protected final NumericColumnAdapter<T> adapter;
  private final boolean allowNullType;
  private final boolean preserveNulls;
  protected final StorageType<R> returnType;

  protected UnaryOperationNumeric(
      final NumericColumnAdapter<T> adapter,
      final boolean allowNullType,
      final boolean preserveNulls,
      final StorageType<R> returnType) {
    this.adapter = adapter;
    this.allowNullType = allowNullType;
    this.preserveNulls = preserveNulls;
    this.returnType = returnType;
  }

  protected R onInvalid(Object value) {
    throw new UnexpectedTypeException(
        adapter.getValidType().toString(), value.getClass().toString());
  }

  @Override
  public boolean canApply(ColumnStorage<?> storage) {
    // Check if can use NullType for left operand.
    if (allowNullType) {
      if (storage.getType() instanceof NullType) {
        return true;
      }
    }

    return adapter.canApply(storage);
  }

  @Override
  public ColumnStorage<R> apply(
      ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    assert canApply(storage);

    if (storage.getType() instanceof NullType) {
      return applyNull(storage, problemAggregator);
    }

    return switch (storage) {
      case ColumnLongStorage longStorage ->
          innerApplySpecializedLongStorage(longStorage, problemAggregator);
      case ColumnDoubleStorage doubleStorage ->
          innerApplySpecializedDoubleStorage(doubleStorage, problemAggregator);
      default -> innerApply(adapter.asTypedStorage(storage), problemAggregator);
    };
  }

  protected ColumnStorage<R> applyNull(
      ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    int checkedSize = Builder.checkSize(storage.getSize());
    var builder = returnType.makeBuilder(checkedSize, problemAggregator);
    builder.appendNulls(checkedSize);
    return builder.seal();
  }

  protected ColumnStorage<R> innerApplySpecializedLongStorage(
      ColumnLongStorage storage, MapOperationProblemAggregator problemAggregator) {
    return StorageIterators.buildOverLongStorage(
        storage,
        preserveNulls,
        returnType.makeBuilder(storage.getSize(), problemAggregator),
        (builder, index, value, isNothing) ->
            doSingleSpecializedLong(builder, index, value, isNothing, problemAggregator));
  }

  @FunctionalInterface
  protected interface SpecializedLongConsumer<R> {
    void accept(
        BuilderForType<R> builder,
        long index,
        long value,
        boolean isNothing,
        MapOperationProblemAggregator problemAggregator);
  }

  protected abstract void doSingleSpecializedLong(
      BuilderForType<R> builder,
      long index,
      long value,
      boolean isNothing,
      MapOperationProblemAggregator problemAggregator);

  protected ColumnStorage<R> innerApplySpecializedDoubleStorage(
      ColumnDoubleStorage storage, MapOperationProblemAggregator problemAggregator) {
    return StorageIterators.buildOverDoubleStorage(
        storage,
        preserveNulls,
        returnType.makeBuilder(storage.getSize(), problemAggregator),
        (builder, index, value, isNothing) ->
            doSingleSpecializedDouble(builder, index, value, isNothing, problemAggregator));
  }

  @FunctionalInterface
  protected interface SpecializedDoubleConsumer<R> {
    void accept(
        BuilderForType<R> builder,
        long index,
        double value,
        boolean isNothing,
        MapOperationProblemAggregator problemAggregator);
  }

  protected abstract void doSingleSpecializedDouble(
      BuilderForType<R> builder,
      long index,
      double value,
      boolean isNothing,
      MapOperationProblemAggregator problemAggregator);

  protected ColumnStorage<R> innerApply(
      ColumnStorage<T> storage, MapOperationProblemAggregator problemAggregator) {
    return StorageIterators.mapOverStorage(
        storage,
        preserveNulls,
        returnType.makeBuilder(storage.getSize(), problemAggregator),
        (index, value) -> doSingle(index, value, problemAggregator));
  }

  protected abstract R doSingle(
      long index, T value, MapOperationProblemAggregator problemAggregator);
}
