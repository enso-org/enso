package org.enso.table.data.column.operation;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageFacade;
import org.enso.table.data.column.storage.numeric.DoubleStorageFacade;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;

public sealed interface NumericColumnAdapter<T>
    permits NumericColumnAdapter.DoubleColumnAdapter,
        NumericColumnAdapter.BigDecimalColumnAdapter,
        NumericColumnAdapter.BigIntegerColumnAdapter,
        NumericColumnAdapter.LongColumnAdapter {
  default boolean canApply(ColumnStorage<?> left) {
    var leftType = left.getType();
    if (getValidType().isOfType(leftType)) {
      return true;
    }

    for (var validInput : getValidInputs()) {
      if (validInput.isOfType(leftType)) {
        return true;
      }
    }

    return false;
  }

  StorageType<T> getValidType();

  StorageType<?>[] getValidInputs();

  ColumnStorage<T> asTypedStorage(ColumnStorage<?> storage);

  final class DoubleColumnAdapter implements NumericColumnAdapter<Double> {
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

  final class BigDecimalColumnAdapter implements NumericColumnAdapter<BigDecimal> {
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

  final class BigIntegerColumnAdapter implements NumericColumnAdapter<BigInteger> {
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

  final class LongColumnAdapter implements NumericColumnAdapter<Long> {
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
