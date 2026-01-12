package org.enso.table.data.column.operation.binary;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.BinaryOperation;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;
import org.enso.table.util.ImmutableBitSet;

public class FillMissingOperation implements BinaryOperation {
  public static FillMissingOperation create(Column column, StorageType<?> resultType) {
    var storage = column.getStorage();
    return switch (storage.getType()) {
      case IntegerType longType -> new LongFillMissingOperation(resultType);
      case FloatType floatType -> new DoubleFillMissingOperation(resultType);
      case BooleanType booleanType -> new BooleanFillMissingOperation(resultType);
      case NullType nullType -> new NullFillMissingOperation(resultType);
      default -> new FillMissingOperation(resultType);
    };
  }

  protected final StorageType<?> resultType;

  private FillMissingOperation(StorageType<?> resultType) {
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
  public ColumnStorage<?> applyMap(
      ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
    if (rightValue == null) {
      return left;
    }

    var resultBuilder = resultType.makeBuilder(left.getSize(), problemAggregator);
    return StorageIterators.buildObjectOverStorage(
        left,
        false,
        resultBuilder,
        (builder, index, value) -> builder.append(value == null ? rightValue : value));
  }

  @Override
  public ColumnStorage<?> applyZip(
      ColumnStorage<?> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    return StorageIterators.zipOverObjectStorages(
        left,
        right,
        s -> resultType.makeBuilder(s, problemAggregator),
        false,
        (idx, l, r) -> l == null ? r : l);
  }

  private static class NullFillMissingOperation extends FillMissingOperation {
    public NullFillMissingOperation(StorageType<?> resultType) {
      super(resultType);
    }

    @Override
    public ColumnStorage<?> applyMap(
        ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
      var typedRightValue = resultType.valueAsType(rightValue);
      return resultType.asTypedStorage(Builder.fromRepeatedItem(typedRightValue, left.getSize()));
    }

    @Override
    public ColumnStorage<?> applyZip(
        ColumnStorage<?> left,
        ColumnStorage<?> right,
        MapOperationProblemAggregator problemAggregator) {
      return resultType.asTypedStorage(right);
    }
  }

  public static class BooleanFillMissingOperation extends FillMissingOperation {
    public static BoolStorage fillMissingBoolStorage(BoolStorage storage, boolean fillValue) {
      var size = (int) storage.getSize();
      var newValues = storage.getValues().cloneBitSet();
      var isNothingMap = storage.getValidityMap().cloneBitSet();
      isNothingMap.flip(0, size);
      if (fillValue != storage.isNegated()) {
        newValues.or(isNothingMap);
      } else {
        newValues.andNot(isNothingMap);
      }
      var validity = ImmutableBitSet.allTrue(size);
      return new BoolStorage(
          new ImmutableBitSet(newValues, size), validity, size, storage.isNegated(), null);
    }

    public BooleanFillMissingOperation(StorageType<?> resultType) {
      super(resultType);
    }

    @Override
    public ColumnStorage<?> applyMap(
        ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
      if (left instanceof BoolStorage boolStorage && rightValue instanceof Boolean rightBool) {
        return fillMissingBoolStorage(boolStorage, rightBool);
      }
      return super.applyMap(left, rightValue, problemAggregator);
    }
  }

  public static class LongFillMissingOperation extends FillMissingOperation {
    public LongFillMissingOperation(StorageType<?> resultType) {
      super(resultType);
    }

    @Override
    public ColumnStorage<?> applyMap(
        ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
      if (left instanceof ColumnLongStorage longStorage) {
        if (NumericConverter.isCoercibleToLong(rightValue)) {
          long rightLongValue = NumericConverter.coerceToLong(rightValue);
          return StorageIterators.buildOverLongStorage(
              longStorage,
              false,
              Builder.getForLong(IntegerType.INT_64, longStorage.getSize(), problemAggregator),
              (builder, index, value, isNothing) ->
                  builder.appendLong(isNothing ? rightLongValue : value));
        } else if (rightValue instanceof BigInteger rightBigInteger) {
          return StorageIterators.buildOverLongStorage(
              longStorage,
              false,
              Builder.getForBigInteger(longStorage.getSize(), problemAggregator),
              (builder, index, value, isNothing) ->
                  builder.append(isNothing ? rightBigInteger : BigInteger.valueOf(value)));
        } else if (rightValue instanceof BigDecimal rightBigDecimal) {
          return StorageIterators.buildOverLongStorage(
              longStorage,
              false,
              Builder.getForBigDecimal(longStorage.getSize()),
              (builder, index, value, isNothing) ->
                  builder.append(isNothing ? rightBigDecimal : BigDecimal.valueOf(value)));
        } else if (NumericConverter.isCoercibleToDouble(rightValue)) {
          double rightDoubleValue = NumericConverter.coerceToDouble(rightValue);
          return StorageIterators.buildOverLongStorage(
              longStorage,
              false,
              Builder.getForDouble(FloatType.FLOAT_64, longStorage.getSize(), problemAggregator),
              (builder, index, value, isNothing) -> {
                if (isNothing) {
                  builder.appendDouble(rightDoubleValue);
                } else {
                  builder.appendLong(value);
                }
              });
        }
      }

      return super.applyMap(left, rightValue, problemAggregator);
    }
  }

  public static class DoubleFillMissingOperation extends FillMissingOperation {
    public DoubleFillMissingOperation(StorageType<?> resultType) {
      super(resultType);
    }

    @Override
    public ColumnStorage<?> applyMap(
        ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
      if (left instanceof ColumnDoubleStorage doubleStorage) {
        if (NumericConverter.isCoercibleToLong(rightValue)) {
          long rightLongValue = NumericConverter.coerceToLong(rightValue);
          return StorageIterators.buildOverDoubleStorage(
              doubleStorage,
              false,
              Builder.getForDouble(FloatType.FLOAT_64, doubleStorage.getSize(), problemAggregator),
              (builder, index, value, isNothing) -> {
                if (isNothing) {
                  builder.appendLong(rightLongValue);
                } else {
                  builder.appendDouble(value);
                }
              });
        } else if (NumericConverter.isCoercibleToDouble(rightValue)
            || rightValue instanceof BigDecimal) {
          double rightDoubleValue = NumericConverter.coerceToDouble(rightValue);
          return StorageIterators.buildOverDoubleStorage(
              doubleStorage,
              false,
              Builder.getForDouble(FloatType.FLOAT_64, doubleStorage.getSize(), problemAggregator),
              (builder, index, value, isNothing) ->
                  builder.appendDouble(isNothing ? rightDoubleValue : value));
        }
      }
      return super.applyMap(left, rightValue, problemAggregator);
    }
  }
}
