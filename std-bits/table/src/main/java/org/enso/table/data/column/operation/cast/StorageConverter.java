package org.enso.table.data.column.operation.cast;

import org.enso.table.data.column.operation.CastProblemBuilder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.*;

public interface StorageConverter<T> {
  Storage<T> cast(Storage<?> storage, CastProblemBuilder problemBuilder);

  static StorageConverter<?> fromStorageType(StorageType storageType) {
    return switch (storageType) {
      case AnyObjectType anyObjectType -> throw new IllegalStateException("TODO");
      case BooleanType booleanType -> throw new IllegalStateException("TODO");
      case DateType dateType -> throw new IllegalStateException("TODO");
      case DateTimeType dateTimeType -> throw new IllegalStateException("TODO");
      case FloatType floatType -> throw new IllegalStateException("TODO");
      case IntegerType integerType -> new ToIntegerStorageConverter(integerType);
      case TextType textType -> throw new IllegalStateException("TODO");
      case TimeOfDayType timeOfDayType -> throw new IllegalStateException("TODO");
    };
  }
}
