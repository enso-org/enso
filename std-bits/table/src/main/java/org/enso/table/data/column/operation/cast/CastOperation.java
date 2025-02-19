package org.enso.table.data.column.operation.cast;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;

/** Perform a cast operation on a Column */
public class CastOperation {
  public static boolean canApply(StorageType sourceType, StorageType targetType) {
    if (targetType instanceof NullType) {
      return false;
    }

    var converter = fromStorageType(targetType);
    return converter.canApply(sourceType);
  }

  public static Column apply(
      Column source, StorageType targetType, ProblemAggregator problemAggregator) {
    if (source.getStorage().getType().equals(targetType)) {
      return source;
    }

    var castProblemAggregator =
        new CastProblemAggregator(problemAggregator, source.getName(), targetType);
    var converter = fromStorageType(targetType);
    var newStorage = converter.cast(source.getStorage(), castProblemAggregator);

    // ToDo: Merge Storage and ColumnStorage
    return new Column(source.getName(), (Storage<?>) newStorage);
  }

  /** Construct a StorageConverter for the given target type. */
  private static StorageConverter<?> fromStorageType(StorageType storageType) {
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
      case NullType nullType -> throw new IllegalArgumentException("Cannot cast to Null type.");
    };
  }
}
