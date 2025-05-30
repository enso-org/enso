package org.enso.table.data.column.operation;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.base.CompareException;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageFacade;
import org.enso.table.data.column.storage.PreciseTypeOptions;
import org.enso.table.data.column.storage.numeric.DoubleStorageFacade;
import org.enso.table.data.column.storage.type.*;
import org.enso.table.data.table.Column;

public abstract class BinaryOperationNumeric<T, R> implements BinaryOperation<R> {
  protected static StorageType<?> storageTypeForObject(Object right) {
    if (right == null) {
      return NullType.INSTANCE;
    }

    if (right instanceof Column rightColumn) {
      return BinaryOperation.getInferredStorage(rightColumn).getType();
    }

    return StorageType.forBoxedItem(right, PreciseTypeOptions.DEFAULT);
  }

  protected final NumericColumnAdapter<T> adapter;
  private final boolean preserveNulls;
  protected final boolean throwOnOther;
  protected final R valueOnOther;
  protected final StorageType<R> returnType;

  protected BinaryOperationNumeric(
      final NumericColumnAdapter<T> adapter,
      final boolean preserveNulls,
      final StorageType<R> returnType) {
    this.adapter = adapter;
    this.preserveNulls = preserveNulls;
    this.returnType = returnType;
    this.throwOnOther = true;
    this.valueOnOther = null;
  }

  protected BinaryOperationNumeric(
      final NumericColumnAdapter<T> adapter,
      final boolean preserveNulls,
      final StorageType<R> returnType,
      final R valueOnOther) {
    this.adapter = adapter;
    this.preserveNulls = preserveNulls;
    this.returnType = returnType;
    this.throwOnOther = false;
    this.valueOnOther = valueOnOther;
  }

  protected R onIncomparable(Object left, Object right) {
    if (throwOnOther) {
      throw new CompareException(left, right);
    }
    return valueOnOther;
  }

  @Override
  public boolean canApplyMap(ColumnStorage<?> left, Object rightValue) {
    var leftType = left.getType();
    if (adapter.getValidType().isOfType(leftType)) {
      return true;
    }

    for (var validInput : adapter.getValidInputs()) {
      if (validInput.isOfType(leftType)) {
        return true;
      }
    }

    return false;
  }

  @Override
  public boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    if (!canApplyMap(left, null)) {
      return false;
    }

    // If not throwing on other types, we can apply the operation
    // Otherwise, we allow Any and Null types on the right or support type.
    if (!throwOnOther) {
      return true;
    }

    var rightType = right.getType();
    return switch (rightType) {
      case NullType nt -> true;
      case AnyObjectType at -> true;
      default -> canApplyMap(right, null);
    };
  }

  @Override
  public ColumnStorage<R> applyMap(
      ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
    assert canApplyMap(left, rightValue);

    if (rightValue == null) {
      return applyNullMap(left, problemAggregator);
    }

    T rightValueTyped = adapter.getValidType().valueAsType(rightValue);
    if (rightValueTyped == null) {
      // If all are Nothing then will return a Nothing Boolean Storage
      return StorageIterators.buildOverStorage(
          left,
          returnType.makeBuilder(left.getSize(), problemAggregator),
          (b, index, value) -> b.append(onIncomparable(value, rightValue)));
    }

    return innerApplyMap(adapter.asTypedStorage(left), rightValueTyped, problemAggregator);
  }

  @Override
  public ColumnStorage<R> applyZip(
      ColumnStorage<?> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    assert canApplyZip(left, right);

    if (NullType.INSTANCE.isOfType(right.getType())) {
      return applyNullMap(left, problemAggregator);
    }

    // Handle the case where right is Any or another type
    if (!adapter.getValidType().isOfType(right.getType())) {
      // Have a mismatch in types (could be AnyObjectType)
      return StorageIterators.zipOverStorages(
          adapter.asTypedStorage(left),
          right,
          size -> returnType.makeBuilder(size, problemAggregator),
          preserveNulls,
          (index, leftValue, rightValue) -> {
            T rightValueTyped = adapter.getValidType().valueAsType(rightValue);
            return rightValue != null && rightValueTyped == null
                ? onIncomparable(leftValue, rightValue)
                : doSingle(leftValue, rightValueTyped, index, problemAggregator);
          });
    }

    return innerApplyZip(
        adapter.asTypedStorage(left), adapter.asTypedStorage(right), problemAggregator);
  }

  protected abstract ColumnStorage<R> applyNullMap(
      ColumnStorage<?> left, MapOperationProblemAggregator problemAggregator);

  protected ColumnStorage<R> innerApplyMap(
      ColumnStorage<T> left, T right, MapOperationProblemAggregator problemAggregator) {
    return StorageIterators.mapOverStorage(
        left,
        preserveNulls,
        returnType.makeBuilder(left.getSize(), problemAggregator),
        (index, value) -> doSingle(value, right, index, problemAggregator));
  }

  protected ColumnStorage<R> innerApplyZip(
      ColumnStorage<T> left,
      ColumnStorage<T> right,
      MapOperationProblemAggregator problemAggregator) {
    return StorageIterators.zipOverStorages(
        left,
        right,
        size -> returnType.makeBuilder(size, problemAggregator),
        preserveNulls,
        (index, x, y) -> doSingle(x, y, index, problemAggregator));
  }

  protected abstract R doSingle(
      T left, T right, long index, MapOperationProblemAggregator problemAggregator);

  protected interface NumericColumnAdapter<T> {
    StorageType<T> getValidType();

    StorageType<?>[] getValidInputs();

    ColumnStorage<T> asTypedStorage(ColumnStorage<?> storage);
  }

  protected static class DoubleColumnAdapter implements NumericColumnAdapter<Double> {
    public static final NumericColumnAdapter<Double> INSTANCE = new DoubleColumnAdapter();

    private static final StorageType<?>[] VALID_TYPES =
        new StorageType[] {BigDecimalType.INSTANCE, BigIntegerType.INSTANCE, IntegerType.INT_64};

    @Override
    public StorageType<Double> getValidType() {
      return FloatType.FLOAT_64;
    }

    @Override
    public StorageType<?>[] getValidInputs() {
      return VALID_TYPES;
    }

    @Override
    public ColumnStorage<Double> asTypedStorage(ColumnStorage<?> storage) {
      return switch (storage.getType()) {
        case FloatType floatType -> floatType.asTypedStorage(storage);
        case BigDecimalType bigDecimalType -> DoubleStorageFacade.forBigDecimal(
            bigDecimalType.asTypedStorage(storage));
        case BigIntegerType bigIntegerType -> DoubleStorageFacade.forBigInteger(
            bigIntegerType.asTypedStorage(storage));
        case IntegerType integerType -> DoubleStorageFacade.forLong(
            integerType.asTypedStorage(storage));
        default -> throw new IllegalArgumentException(
            "Unsupported storage type: " + storage.getType());
      };
    }
  }

  protected static class BigDecimalColumnAdapter implements NumericColumnAdapter<BigDecimal> {
    public static final NumericColumnAdapter<BigDecimal> INSTANCE = new BigDecimalColumnAdapter();

    private static final StorageType<?>[] VALID_TYPES =
        new StorageType[] {BigIntegerType.INSTANCE, IntegerType.INT_64};

    @Override
    public StorageType<BigDecimal> getValidType() {
      return BigDecimalType.INSTANCE;
    }

    @Override
    public StorageType<?>[] getValidInputs() {
      return VALID_TYPES;
    }

    @Override
    public ColumnStorage<BigDecimal> asTypedStorage(ColumnStorage<?> storage) {
      return switch (storage.getType()) {
        case BigDecimalType bigDecimalType -> bigDecimalType.asTypedStorage(storage);
        case BigIntegerType bigIntegerType -> new ColumnStorageFacade<>(
            bigIntegerType.asTypedStorage(storage), BigDecimal::new);
        case IntegerType integerType -> new ColumnStorageFacade<>(
            integerType.asTypedStorage(storage), BigDecimal::valueOf);
        default -> throw new IllegalArgumentException(
            "Unsupported storage type: " + storage.getType());
      };
    }
  }

  protected static class BigIntegerColumnAdapter implements NumericColumnAdapter<BigInteger> {
    public static final NumericColumnAdapter<BigInteger> INSTANCE = new BigIntegerColumnAdapter();

    private static final StorageType<?>[] VALID_TYPES = new StorageType[] {IntegerType.INT_64};

    @Override
    public StorageType<BigInteger> getValidType() {
      return BigIntegerType.INSTANCE;
    }

    @Override
    public StorageType<?>[] getValidInputs() {
      return VALID_TYPES;
    }

    @Override
    public ColumnStorage<BigInteger> asTypedStorage(ColumnStorage<?> storage) {
      return switch (storage.getType()) {
        case BigIntegerType bigIntegerType -> bigIntegerType.asTypedStorage(storage);
        case IntegerType integerType -> new ColumnStorageFacade<>(
            integerType.asTypedStorage(storage), BigInteger::valueOf);
        default -> throw new IllegalArgumentException(
            "Unsupported storage type: " + storage.getType());
      };
    }
  }

  protected static class LongColumnAdapter implements NumericColumnAdapter<Long> {
    public static final NumericColumnAdapter<Long> INSTANCE = new LongColumnAdapter();

    private static final StorageType<?>[] VALID_TYPES = new StorageType[0];

    @Override
    public StorageType<Long> getValidType() {
      return IntegerType.INT_64;
    }

    @Override
    public StorageType<?>[] getValidInputs() {
      return VALID_TYPES;
    }

    @Override
    public ColumnLongStorage asTypedStorage(ColumnStorage<?> storage) {
      if (storage.getType() instanceof IntegerType integerType) {
        return integerType.asTypedStorage(storage);
      }
      throw new IllegalArgumentException("Unsupported storage type: " + storage.getType());
    }
  }
}
