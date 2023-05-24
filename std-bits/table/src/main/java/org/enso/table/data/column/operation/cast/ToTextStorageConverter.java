package org.enso.table.data.column.operation.cast;

import org.enso.polyglot.common_utils.Core_Date_Utils;
import org.enso.table.data.column.builder.object.StringBuilder;
import org.enso.table.data.column.storage.*;
import org.enso.table.data.column.storage.type.AnyObjectType;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.function.Function;

public class ToTextStorageConverter implements StorageConverter<String> {
  public Storage<String> cast(Storage<?> storage, CastProblemBuilder problemBuilder) {
    if (storage instanceof StringStorage stringStorage) {
      return stringStorage;
    } if (storage instanceof LongStorage longStorage) {
      return castLongStorage(longStorage);
    } else if (storage instanceof DoubleStorage doubleStorage) {
      return castDoubleStorage(doubleStorage);
    } else if (storage instanceof BoolStorage boolStorage) {
      return castBoolStorage(boolStorage);
    } else if (storage instanceof TimeOfDayStorage timeOfDayStorage) {
      return castDateTimeStorage(timeOfDayStorage, this::convertTime);
    } else if (storage instanceof DateStorage dateStorage) {
      return castDateTimeStorage(dateStorage, this::convertDate);
    } else if (storage instanceof DateTimeStorage dateTimeStorage) {
      return castDateTimeStorage(dateTimeStorage, this::convertDateTime);
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemBuilder);
    } else {
      throw new IllegalStateException("No known strategy for casting storage " + storage + " to Integer.");
    }
  }

  public Storage<String> castFromMixed(Storage<?> mixedStorage, CastProblemBuilder problemBuilder) {
    StringBuilder builder = new StringBuilder(mixedStorage.size());
    for (int i = 0; i < mixedStorage.size(); i++) {
      Object o = mixedStorage.getItemBoxed(i);
      switch (o) {
        case null -> builder.appendNulls(1);
        case LocalTime d -> builder.append(convertTime(d));
        case LocalDate d -> builder.append(convertDate(d));
        case ZonedDateTime d -> builder.append(convertDateTime(d));
        case Boolean b -> builder.append(convertBoolean(b));
        default -> builder.append(o.toString());
      }
    }

    return builder.seal();
  }

  private final DateTimeFormatter dateFormatter = Core_Date_Utils.defaultLocalDateFormatter();
  private final DateTimeFormatter timeFormatter = Core_Date_Utils.defaultLocalTimeFormatter();
  private final DateTimeFormatter dateTimeFormatter = Core_Date_Utils.defaultZonedDateTimeFormatter();


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

  private Storage<String> castLongStorage(LongStorage longStorage) {
    StringBuilder builder = new StringBuilder(longStorage.size());
    for (int i = 0; i < longStorage.size(); i++) {
      if (longStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        long value = longStorage.getItem(i);
        builder.append(Long.toString(value));
      }
    }
    return builder.seal();
  }

  private Storage<String> castBoolStorage(BoolStorage boolStorage) {
    StringBuilder builder = new StringBuilder(boolStorage.size());
    for (int i = 0; i < boolStorage.size(); i++) {
      if (boolStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        boolean value = boolStorage.getItem(i);
        builder.append(convertBoolean(value));
      }
    }
    return builder.seal();
  }

  private Storage<String> castDoubleStorage(DoubleStorage doubleStorage) {
    StringBuilder builder = new StringBuilder(doubleStorage.size());
    for (int i = 0; i < doubleStorage.size(); i++) {
      if (doubleStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        double value = doubleStorage.getItem(i);
        builder.append(Double.toString(value));
      }
    }
    return builder.seal();
  }

  private <T> Storage<String> castDateTimeStorage(Storage<T> storage, Function<T, String> converter) {
    StringBuilder builder = new StringBuilder(storage.size());
    for (int i = 0; i < storage.size(); i++) {
      if (storage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        T value = storage.getItemBoxed(i);
        builder.append(converter.apply(value));
      }
    }
    return builder.seal();
  }
}
