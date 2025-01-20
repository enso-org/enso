package org.enso.table.data.column.operation.cast;

import java.util.function.IntFunction;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.graalvm.polyglot.Context;

/** A strategy for converting storages to a specific target type. */
public interface StorageConverter<T> {
  /** Convert a given storage to the target type of this converter, reporting any problems. */
  Storage<T> cast(Storage<?> storage, CastProblemAggregator problemAggregator);

  /** Construct a StorageConverter for the given target type. */
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
      case BigDecimalType bigDecimalType -> new ToBigDecimalConverter();
    };
  }

  static <T> Storage<T> innerLoop(
      BuilderForType<T> builder, ColumnStorage storage, IntFunction<T> converter) {
    Context context = Context.getCurrent();

    long n = storage.getSize();
    if (n > Integer.MAX_VALUE) {
      throw new IllegalArgumentException(
          "Cannot currently operate on columns larger than " + Integer.MAX_VALUE + ".");
    }

    for (int i = 0; i < n; i++) {
      if (storage.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        builder.appendNoGrow(converter.apply(i));
      }

      context.safepoint();
    }

    return builder.seal();
  }
}
