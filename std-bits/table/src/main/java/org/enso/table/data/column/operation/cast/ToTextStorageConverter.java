package org.enso.table.data.column.operation.cast;

import org.enso.base.Text_Utils;
import org.enso.polyglot.common_utils.Core_Date_Utils;
import org.enso.table.data.column.builder.object.StringBuilder;
import org.enso.table.data.column.storage.*;
import org.enso.table.data.column.storage.datetime.DateStorage;
import org.enso.table.data.column.storage.datetime.DateTimeStorage;
import org.enso.table.data.column.storage.datetime.TimeOfDayStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.TextType;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.function.Function;

public class ToTextStorageConverter implements StorageConverter<String> {
  private final int minLength;
  private final int maxLength;
  private final TextType targetType;

  public ToTextStorageConverter(TextType textType) {
    maxLength = Math.toIntExact(textType.maxLength());
    if (textType.fixedLength()) {
      minLength = maxLength;
    } else {
      minLength = -1;
    }

    targetType = textType;
  }

  public Storage<String> cast(Storage<?> storage, CastProblemBuilder problemBuilder) {
    if (storage instanceof StringStorage stringStorage) {
      if (stringStorage.getType().equals(targetType)) {
        return stringStorage;
      } else {
        return adaptStringStorage(stringStorage);
      }
    }
    if (storage instanceof LongStorage longStorage) {
      return castLongStorage(longStorage, problemBuilder);
    } else if (storage instanceof DoubleStorage doubleStorage) {
      return castDoubleStorage(doubleStorage, problemBuilder);
    } else if (storage instanceof BoolStorage boolStorage) {
      return castBoolStorage(boolStorage, problemBuilder);
    } else if (storage instanceof TimeOfDayStorage timeOfDayStorage) {
      return castDateTimeStorage(timeOfDayStorage, this::convertTime, problemBuilder);
    } else if (storage instanceof DateStorage dateStorage) {
      return castDateTimeStorage(dateStorage, this::convertDate, problemBuilder);
    } else if (storage instanceof DateTimeStorage dateTimeStorage) {
      return castDateTimeStorage(dateTimeStorage, this::convertDateTime, problemBuilder);
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
        case LocalTime d -> builder.append(adapt(convertTime(d), problemBuilder));
        case LocalDate d -> builder.append(adapt(convertDate(d), problemBuilder));
        case ZonedDateTime d -> builder.append(adapt(convertDateTime(d), problemBuilder));
        case Boolean b -> builder.append(adapt(convertBoolean(b), problemBuilder));
        default -> builder.append(adapt(o.toString(), problemBuilder));
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

  private Storage<String> castLongStorage(LongStorage longStorage, CastProblemBuilder problemBuilder) {
    StringBuilder builder = new StringBuilder(longStorage.size());
    for (int i = 0; i < longStorage.size(); i++) {
      if (longStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        long value = longStorage.getItem(i);
        builder.append(adapt(Long.toString(value), problemBuilder));
      }
    }
    return builder.seal();
  }

  private Storage<String> castBoolStorage(BoolStorage boolStorage, CastProblemBuilder problemBuilder) {
    StringBuilder builder = new StringBuilder(boolStorage.size());
    for (int i = 0; i < boolStorage.size(); i++) {
      if (boolStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        boolean value = boolStorage.getItem(i);
        builder.append(adapt(convertBoolean(value), problemBuilder));
      }
    }
    return builder.seal();
  }

  private Storage<String> castDoubleStorage(DoubleStorage doubleStorage, CastProblemBuilder problemBuilder) {
    StringBuilder builder = new StringBuilder(doubleStorage.size());
    for (int i = 0; i < doubleStorage.size(); i++) {
      if (doubleStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        double value = doubleStorage.getItem(i);
        builder.append(adapt(Double.toString(value), problemBuilder));
      }
    }
    return builder.seal();
  }

  private <T> Storage<String> castDateTimeStorage(Storage<T> storage, Function<T, String> converter, CastProblemBuilder problemBuilder) {
    StringBuilder builder = new StringBuilder(storage.size());
    for (int i = 0; i < storage.size(); i++) {
      if (storage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        T value = storage.getItemBoxed(i);
        String converted = converter.apply(value);
        builder.append(adapt(converted, problemBuilder));
      }
    }
    return builder.seal();
  }

  private String adapt(String value, CastProblemBuilder problemBuilder) {
    String adapted = adaptWithoutWarning(value);

    // If the value was truncated, report the data loss.
    if (adapted.length() < value.length()) {
      problemBuilder.reportTextTooLong(value);
    }

    return adapted;
  }

  private String adaptWithoutWarning(String value) {
    if (maxLength == -1) {
      return value;
    }

    int textLength = (int) Text_Utils.grapheme_length(value);

    if (textLength > maxLength) {
      return Text_Utils.take_prefix(value, maxLength);
    } else if (textLength < minLength) {
      return value + " ".repeat(minLength - textLength);
    } else {
      return value;
    }
  }

  private Storage<String> adaptStringStorage(StringStorage stringStorage) {
    StringBuilder builder = new StringBuilder(stringStorage.size());
    for (int i = 0; i < stringStorage.size(); i++) {
      if (stringStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        String value = stringStorage.getItem(i);
        // Adapting an existing string storage into a new type is done without warnings.
        builder.append(adaptWithoutWarning(value));
      }
    }
    return builder.seal();
  }
}
