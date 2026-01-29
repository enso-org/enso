package org.enso.table.data.column.storage;

import java.util.function.LongFunction;
import org.enso.table.data.column.storage.iterators.*;
import org.enso.table.data.column.storage.type.*;

public class ColumnStorageProxy<T> extends AbstractBaseStorage<T> {
  private final long size;
  protected final ColumnStorage<?> originalStorage;
  private final LongFunction<T> getter;

  public static <T> ColumnStorage<T> create(
      StorageType<T> storageType, long size, ColumnStorage<?> originalStorage) {
    return switch (storageType) {
      case FloatType _ ->
          storageType.asTypedStorage(new ColumnDoubleStorageProxy(size, originalStorage));
      case IntegerType integerType ->
          storageType.asTypedStorage(
              new ColumnLongStorageProxy(integerType, size, originalStorage));
      case BooleanType _ ->
          storageType.asTypedStorage(new ColumnBooleanStorageProxy(size, originalStorage));
      default -> {
        LongFunction<T> getter =
            switch (storageType) {
              case NullType _ -> index -> null;
              case BigDecimalType _ ->
                  index -> {
                    var asString = originalStorage.getItemAsString(index);
                    return asString == null ? null : storageType.valueAsType(asString);
                  };
              default -> index -> storageType.valueAsType(originalStorage.getItemBoxed(index));
            };
        yield new ColumnStorageProxy<>(storageType, size, originalStorage, getter);
      }
    };
  }

  private ColumnStorageProxy(
      StorageType<T> storageType,
      long size,
      ColumnStorage<?> originalStorage,
      LongFunction<T> getter) {
    super(storageType);
    this.size = size;
    this.originalStorage = originalStorage;
    this.getter = getter;
  }

  @Override
  public long getSize() {
    return size;
  }

  @Override
  public boolean isNothing(long index) {
    if (index < 0 || index >= size) {
      throw new IndexOutOfBoundsException(
          "Index " + index + " is out of bounds for column of size " + size);
    }
    return originalStorage.isNothing(index);
  }

  @Override
  public T getItemBoxed(long index) {
    if (index < 0 || index >= size) {
      throw new IndexOutOfBoundsException(
          "Index " + index + " is out of bounds for column of size " + size);
    }
    return getter.apply(index);
  }

  private static class ColumnDoubleStorageProxy extends ColumnStorageProxy<Double>
      implements ColumnDoubleStorage {
    public ColumnDoubleStorageProxy(long size, ColumnStorage<?> originalStorage) {
      super(
          FloatType.FLOAT_64,
          size,
          originalStorage,
          idx -> FloatType.FLOAT_64.valueAsType(originalStorage.getItemBoxed(idx)));
    }

    @Override
    public double getItemAsDouble(long index) throws ValueIsNothingException {
      var value = originalStorage.getItemBoxed(index);
      if (value == null) {
        throw new ValueIsNothingException(index);
      }
      return FloatType.FLOAT_64.valueAsType(value);
    }

    @Override
    public ColumnDoubleStorageIterator iteratorWithIndex() {
      return new DoubleStorageIterator(this);
    }
  }

  private static class ColumnLongStorageProxy extends ColumnStorageProxy<Long>
      implements ColumnLongStorage {
    public ColumnLongStorageProxy(
        IntegerType integerType, long size, ColumnStorage<?> originalStorage) {
      super(
          integerType,
          size,
          originalStorage,
          idx -> integerType.valueAsType(originalStorage.getItemBoxed(idx)));
    }

    @Override
    public long getItemAsLong(long index) throws ValueIsNothingException {
      var value = originalStorage.getItemBoxed(index);
      if (value == null) {
        throw new ValueIsNothingException(index);
      }
      return IntegerType.INT_64.valueAsType(value);
    }

    @Override
    public ColumnLongStorageIterator iteratorWithIndex() {
      return new LongStorageIterator(this);
    }
  }

  private static class ColumnBooleanStorageProxy extends ColumnStorageProxy<Boolean>
      implements ColumnBooleanStorage {
    public ColumnBooleanStorageProxy(long size, ColumnStorage<?> originalStorage) {
      super(
          BooleanType.INSTANCE,
          size,
          originalStorage,
          idx -> BooleanType.INSTANCE.valueAsType(originalStorage.getItemBoxed(idx)));
    }

    @Override
    public boolean getItemAsBoolean(long index) throws ValueIsNothingException {
      var value = originalStorage.getItemBoxed(index);
      if (value == null) {
        throw new ValueIsNothingException(index);
      }
      return BooleanType.INSTANCE.valueAsType(value);
    }

    @Override
    public ColumnBooleanStorageIterator iteratorWithIndex() {
      return new BooleanStorageIterator(this);
    }
  }
}
