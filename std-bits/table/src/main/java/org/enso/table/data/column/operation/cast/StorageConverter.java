package org.enso.table.data.column.operation.cast;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;

/**
 * A strategy for converting storages to a specific target type.
 */
public interface StorageConverter<T> {
  /**
   * Convert a given storage to the target type of this converter, reporting any problems.
   */
  Storage<T> cast(Storage<?> storage, CastProblemAggregator problemAggregator);

  /**
   * Construct a StorageConverter for the given target type.
   */
  static StorageConverter<?> fromStorageType(StorageType storageType) {
    return switch (storageType) {
      case AnyObjectType anyObjectType -> new ToMixedStorageConverter();
      case BooleanType booleanType -> new ToBooleanStorageConverter();
      case DateType dateType -> new ToDateStorageConverter();
      case DateTimeType dateTimeType -> new ToDateTimeStorageConverter();
      case FloatType floatType -> new ToFloatStorageConverter(floatType);
      case IntegerType integerType -> new ToIntegerStorageConverter(integerType);
      case TextType textType -> new ToTextStorageConverter(textType);
      case TimeOfDayType timeOfDayType -> new ToTimeOfDayStorageConverter();
      case BigIntegerType bigIntegerType -> new ToBigIntegerConverter();
    };
  }
}
