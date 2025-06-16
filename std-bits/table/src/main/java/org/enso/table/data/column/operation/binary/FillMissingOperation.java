package org.enso.table.data.column.operation.binary;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.BinaryOperation;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.*;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

import java.util.BitSet;

public class FillMissingOperation<T> implements BinaryOperation<T> {
  public static <T> FillMissingOperation<T> create(Column column, StorageType<T> resultType) {
    var storage = column.getStorage();
    return switch (storage.getType()) {
      case IntegerType longType -> new LongFillMissingOperation<>(resultType);
      case BooleanType booleanType -> new BooleanFillMissingOperation<>(resultType);
      case NullType nullType -> new NullFillMissingOperation<>(resultType);
      default -> new FillMissingOperation<>(resultType);
    };
  }

  protected final StorageType<T> resultType;

  private FillMissingOperation(StorageType<T> resultType) {
    this.resultType = resultType;
  }

  @Override
  public boolean canApplyMap(ColumnStorage<?> left, Object rightValue) {
    return true;
  }

  @Override
  public boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    return true;
  }

  @Override
  public ColumnStorage<T> applyMap(ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
    var resultBuilder = resultType.makeBuilder(left.getSize(), problemAggregator);
    var rawStorage = StorageIterators.buildObjectOverStorage(
        left,
        false,
        resultBuilder,
        (builder, index, value) -> builder.append(value == null ? rightValue : value));
    return resultType.asTypedStorage(rawStorage);
  }

  @Override
  public ColumnStorage<T> applyZip(ColumnStorage<?> left, ColumnStorage<?> right, MapOperationProblemAggregator problemAggregator) {
    var rawStorage = StorageIterators.zipOverObjectStorages(
        left,
        right,
        s -> resultType.makeBuilder(s, problemAggregator),
        false,
        (idx, l, r) -> l == null ? r : l);
    return resultType.asTypedStorage(rawStorage);
  }

  private static class NullFillMissingOperation<T> extends FillMissingOperation<T> {
    public NullFillMissingOperation(StorageType<T> resultType) {
      super(resultType);
    }

    @Override
    public ColumnStorage<T> applyMap(ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
      return resultType.asTypedStorage(Builder.fromRepeatedItem(rightValue, left.getSize()));
    }

    @Override
    public ColumnStorage<T> applyZip(ColumnStorage<?> left, ColumnStorage<?> right, MapOperationProblemAggregator problemAggregator) {
      return resultType.asTypedStorage(right);
    }
  }

  public static class BooleanFillMissingOperation<T> extends FillMissingOperation<T> {
    public static BoolStorage fillMissingBoolStorage(
        BoolStorage storage, boolean fillValue) {
      final var newValues = (BitSet) storage.getValues().clone();
      if (fillValue != storage.isNegated()) {
        newValues.or(storage.getIsNothingMap());
      } else {
        newValues.andNot(storage.getIsNothingMap());
      }
      return new BoolStorage(newValues, new BitSet(), (int)storage.getSize(), storage.isNegated());
    }

    public BooleanFillMissingOperation(StorageType<T> resultType) {
      super(resultType);
    }

    @Override
    public ColumnStorage<T> applyMap(ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
      if (left instanceof BoolStorage boolStorage && rightValue instanceof Boolean rightBool) {
        return resultType.asTypedStorage(fillMissingBoolStorage(boolStorage, rightBool));
      }
      return super.applyMap(left, rightValue, problemAggregator);
    }

    @Override
    public ColumnStorage<T> applyZip(ColumnStorage<?> left, ColumnStorage<?> right, MapOperationProblemAggregator problemAggregator) {
      return resultType.asTypedStorage(right);
    }
  }

  public static class LongFillMissingOperation<T> extends FillMissingOperation<T> {
    public LongFillMissingOperation(StorageType<T> resultType) {
      super(resultType);
    }

    @Override
    public ColumnStorage<T> applyMap(ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
      if (left instanceof ColumnLongStorage longStorage) {
        if (NumericConverter.isCoercibleToLong(rightValue)) {
          var raw = StorageIterators.mapOverLongStorage(
              longStorage,
              Builder.getForLong(resultType, longStorage.getSize(), problemAggregator),
              (builder, index, value) -> {
                if (value == null) {
                  builder.appendLong(NumericConverter.toLong(rightValue));
                } else {
                  builder.appendLong(value);
                }
              });
          )
        }
      }
      return super.applyMap(left, rightValue, problemAggregator);
    }

    @Override
    public ColumnStorage<T> applyZip(ColumnStorage<?> left, ColumnStorage<?> right, MapOperationProblemAggregator problemAggregator) {
      return resultType.asTypedStorage(right);
    }
  }
}
