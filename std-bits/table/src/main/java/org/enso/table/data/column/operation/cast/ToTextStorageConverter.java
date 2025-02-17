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
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.data.column.storage.datetime.DateStorage;
import org.enso.table.data.column.storage.datetime.DateTimeStorage;
import org.enso.table.data.column.storage.datetime.TimeOfDayStorage;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;

public class ToTextStorageConverter implements StorageConverter<String> {
  private final TextType targetType;

  public ToTextStorageConverter(TextType textType) {
    targetType = textType;
  }

  @Override
  public boolean canApply(StorageType sourceType) {
    return true;
  }

  @Override
  public ColumnStorage<String> cast(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof StringStorage stringStorage) {
      if (canAvoidCopying(stringStorage)) {
        return retypeStringStorage(stringStorage);
      } else {
        return adaptStringStorage(stringStorage);
      }
    }
    if (storage instanceof ColumnLongStorage longStorage) {
      return castLongStorage(longStorage, problemAggregator);
    } else if (storage instanceof ColumnDoubleStorage doubleStorage) {
      return castDoubleStorage(doubleStorage, problemAggregator);
    } else if (storage instanceof ColumnBooleanStorage boolStorage) {
      return castBoolStorage(boolStorage, problemAggregator);
    } else if (storage instanceof TimeOfDayStorage timeOfDayStorage) {
      return castTemporalStorage(timeOfDayStorage, this::convertTime, problemAggregator);
    } else if (storage instanceof DateStorage dateStorage) {
      return castTemporalStorage(dateStorage, this::convertDate, problemAggregator);
    } else if (storage instanceof DateTimeStorage dateTimeStorage) {
      return castTemporalStorage(dateTimeStorage, this::convertDateTime, problemAggregator);
    } else {
      return castFromObject(storage, problemAggregator);
    }
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

  private ColumnStorage<String> adaptStringStorage(StringStorage stringStorage) {
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

  private boolean canAvoidCopying(StringStorage stringStorage) {
    if (targetType.fitsExactly(stringStorage.getType())) {
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
  private ColumnStorage<String> retypeStringStorage(StringStorage stringStorage) {
    return new StringStorage(stringStorage.getData(), targetType);
  }
}
