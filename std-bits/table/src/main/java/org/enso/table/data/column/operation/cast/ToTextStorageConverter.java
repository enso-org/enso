package org.enso.table.data.column.operation.cast;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.function.Function;
import org.enso.polyglot.common_utils.Core_Date_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.TypedStorage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;

public class ToTextStorageConverter implements StorageConverter<String> {
  private final TextType targetType;

  public ToTextStorageConverter(TextType textType) {
    targetType = textType;
  }

  @Override
  public boolean canApply(StorageType<?> sourceType) {
    return true;
  }

  @Override
  public ColumnStorage<String> cast(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    var storageType = StorageType.ofStorage(storage);
    return switch (storageType) {
      case NullType _ -> Builder.makeEmpty(targetType, storage.getSize());
      case TextType textType -> {
        var textTypedStorage = textType.asTypedStorage(storage);
        if (storage instanceof TypedStorage<?> typedStorage && canAvoidCopying(textTypedStorage)) {
          @SuppressWarnings("unchecked")
          var stringStorage = (TypedStorage<String>) typedStorage;
          yield retypeStringStorage(stringStorage);
        }
        yield adaptStringStorage(textTypedStorage);
      }
      case IntegerType it -> castLongStorage(it.asTypedStorage(storage), problemAggregator);
      case FloatType ft -> castDoubleStorage(ft.asTypedStorage(storage), problemAggregator);
      case BooleanType bt -> castBoolStorage(bt.asTypedStorage(storage), problemAggregator);
      case TimeOfDayType timeOfDayType ->
          castTemporalStorage(
              timeOfDayType.asTypedStorage(storage), this::convertTime, problemAggregator);
      case DateType dateType ->
          castTemporalStorage(
              dateType.asTypedStorage(storage), this::convertDate, problemAggregator);
      case DateTimeType dateTimeType ->
          castTemporalStorage(
              dateTimeType.asTypedStorage(storage), this::convertDateTime, problemAggregator);
      default -> castFromObject(storage, problemAggregator);
    };
  }

  private ColumnStorage<String> castFromObject(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    return StorageIterators.mapOverStorage(
        storage,
        Builder.getForText(targetType, storage.getSize()),
        (index, value) ->
            switch (value) {
              case LocalTime d -> adapt(convertTime(d), problemAggregator);
              case LocalDate d -> adapt(convertDate(d), problemAggregator);
              case ZonedDateTime d -> adapt(convertDateTime(d), problemAggregator);
              case Boolean b -> adapt(convertBoolean(b), problemAggregator);
              default -> adapt(value.toString(), problemAggregator);
            });
  }

  private ColumnStorage<String> castLongStorage(
      ColumnLongStorage longStorage, CastProblemAggregator problemAggregator) {
    return StorageIterators.mapOverLongStorage(
        longStorage,
        Builder.getForText(targetType, longStorage.getSize()),
        (index, value, isNothing) -> adapt(Long.toString(value), problemAggregator));
  }

  private ColumnStorage<String> castBoolStorage(
      ColumnBooleanStorage boolStorage, CastProblemAggregator problemAggregator) {
    return StorageIterators.mapOverBooleanStorage(
        boolStorage,
        Builder.getForText(targetType, boolStorage.getSize()),
        (index, value, isNothing) -> adapt(convertBoolean(value), problemAggregator));
  }

  private ColumnStorage<String> castDoubleStorage(
      ColumnDoubleStorage doubleStorage, CastProblemAggregator problemAggregator) {
    return StorageIterators.mapOverDoubleStorage(
        doubleStorage,
        Builder.getForText(targetType, doubleStorage.getSize()),
        (index, value, isNothing) -> adapt(Double.toString(value), problemAggregator));
  }

  private <T> ColumnStorage<String> castTemporalStorage(
      ColumnStorage<T> storage,
      Function<T, String> converter,
      CastProblemAggregator problemAggregator) {
    return StorageIterators.mapOverStorage(
        storage,
        Builder.getForText(targetType, storage.getSize()),
        (index, value) -> adapt(converter.apply(value), problemAggregator));
  }

  private ColumnStorage<String> adaptStringStorage(ColumnStorage<String> stringStorage) {
    // Adapting an existing string storage into a new type is done without warnings.
    return StorageIterators.mapOverStorage(
        stringStorage,
        Builder.getForText(targetType, stringStorage.getSize()),
        (index, value) -> adaptWithoutWarning(value));
  }

  private final DateTimeFormatter dateFormatter = Core_Date_Utils.defaultLocalDateFormatter;
  private final DateTimeFormatter timeFormatter = Core_Date_Utils.defaultLocalTimeFormatter;
  private final DateTimeFormatter dateTimeFormatter = Core_Date_Utils.defaultZonedDateTimeFormatter;

  private String convertDate(LocalDate date) {
    return date.format(dateFormatter);
  }

  private String convertTime(LocalTime time) {
    return time.format(timeFormatter);
  }

  private String convertDateTime(ZonedDateTime dateTime) {
    return dateTime.format(dateTimeFormatter);
  }

  private String convertBoolean(Boolean b) {
    return b ? "True" : "False";
  }

  private String adapt(String value, CastProblemAggregator problemAggregator) {
    String adapted = adaptWithoutWarning(value);

    // If the value was truncated, report the data loss.
    // (We can use the codepoint lengths here because truncation on grapheme length will still
    // change the codepoint
    // length too, and this check is simply faster.)
    if (adapted.length() < value.length()) {
      problemAggregator.reportTextTooLong(value);
    }

    return adapted;
  }

  private String adaptWithoutWarning(String value) {
    return targetType.adapt(value);
  }

  private boolean canAvoidCopying(ColumnStorage<String> stringStorage) {
    var type = StorageType.ofStorage(stringStorage);
    if (type instanceof TextType textType && targetType.fitsExactly(textType)) {
      return true;
    }

    long maxLength = Long.MIN_VALUE;
    long minLength = Long.MAX_VALUE;
    for (long i = 0; i < stringStorage.getSize(); i++) {
      String value = stringStorage.getItemBoxed(i);
      if (value == null) {
        continue;
      }

      long length = value.length();
      if (length > maxLength) {
        maxLength = length;
      }
      if (length < minLength) {
        minLength = length;
      }
    }

    if (targetType.fixedLength()) {
      boolean effectivelyFixedLength = minLength == maxLength;
      return effectivelyFixedLength && targetType.maxLength() == maxLength;
    } else {
      return targetType.maxLength() == -1 || maxLength <= targetType.maxLength();
    }
  }

  /**
   * Creates a new storage re-using the existing array.
   *
   * <p>This can only be done if the values do not need any adaptations, checked by {@code
   * canAvoidCopying}.
   */
  private ColumnStorage<String> retypeStringStorage(TypedStorage<String> stringStorage) {
    return new TypedStorage<>(targetType, stringStorage.getData());
  }
}
