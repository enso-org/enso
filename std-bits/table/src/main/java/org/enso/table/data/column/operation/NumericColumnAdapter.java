package org.enso.table.data.column.operation;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Iterator;
import java.util.function.ToDoubleFunction;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageFacade;
import org.enso.table.data.column.storage.ValueIsNothingException;
import org.enso.table.data.column.storage.iterators.ColumnDoubleStorageIterator;
import org.enso.table.data.column.storage.iterators.DoubleStorageIterator;
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
        case BigDecimalType bigDecimalType ->
            new DoubleStorageFacade<>(
                bigDecimalType.asTypedStorage(storage), BigDecimal::doubleValue);
        case BigIntegerType bigIntegerType ->
            new DoubleStorageFacade<>(
                bigIntegerType.asTypedStorage(storage), BigInteger::doubleValue);
        case IntegerType integerType ->
            new DoubleStorageFacade<>(integerType.asTypedStorage(storage), Long::doubleValue);
        default ->
            throw new IllegalArgumentException("Unsupported storage type: " + storage.getType());
      };
    }

    /** A facade for a column storage that converts the stored type to a double. */
    private record DoubleStorageFacade<T>(ColumnStorage<T> parent, ToDoubleFunction<T> converter)
        implements ColumnDoubleStorage {

      @Override
      public long uniqueKey() {
        return parent.uniqueKey();
      }

      @Override
      public double getItemAsDouble(long index) throws ValueIsNothingException {
        if (isNothing(index)) {
          throw new ValueIsNothingException(index);
        }
        T item = parent.getItemBoxed(index);
        return converter.applyAsDouble(item);
      }

      @Override
      public long getSize() {
        return parent.getSize();
      }

      @Override
      public FloatType getType() {
        return FloatType.FLOAT_64;
      }

      @Override
      public boolean isNothing(long index) {
        return parent.isNothing(index);
      }

      @Override
      public Double getItemBoxed(long index) {
        T item = parent.getItemBoxed(index);
        return item == null ? null : converter.applyAsDouble(item);
      }

      @Override
      public Iterator<Double> iterator() {
        return new Iterator<>() {
          private final Iterator<T> parentIterator = parent.iterator();

          @Override
          public boolean hasNext() {
            return parentIterator.hasNext();
          }

          @Override
          public Double next() {
            T item = parentIterator.next();
            return item == null ? null : converter.applyAsDouble(item);
          }
        };
      }

      @Override
      public ColumnDoubleStorageIterator iteratorWithIndex() {
        return new DoubleStorageIterator(this);
      }
    }
  }

  final class BigDecimalColumnAdapter implements NumericColumnAdapter<BigDecimal> {
    public static final NumericColumnAdapter<BigDecimal> INSTANCE = new BigDecimalColumnAdapter();

    private static final StorageType<?>[] VALID_TYPES =
        new StorageType[] {FloatType.FLOAT_64, BigIntegerType.INSTANCE, IntegerType.INT_64};

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
        case BigIntegerType bigIntegerType ->
            new ColumnStorageFacade<>(bigIntegerType.asTypedStorage(storage), BigDecimal::new);
        case FloatType floatType ->
            new ColumnStorageFacade<>(floatType.asTypedStorage(storage), BigDecimal::valueOf);
        case IntegerType integerType ->
            new ColumnStorageFacade<>(integerType.asTypedStorage(storage), BigDecimal::valueOf);
        default ->
            throw new IllegalArgumentException("Unsupported storage type: " + storage.getType());
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
        case IntegerType integerType ->
            new ColumnStorageFacade<>(integerType.asTypedStorage(storage), BigInteger::valueOf);
        default ->
            throw new IllegalArgumentException("Unsupported storage type: " + storage.getType());
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
