package org.enso.table.data.column.operation.cast;

import org.enso.polyglot.common_utils.Core_Date_Utils;
import org.enso.table.data.column.builder.StringBuilder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.data.column.storage.datetime.DateStorage;
import org.enso.table.data.column.storage.datetime.DateTimeStorage;
import org.enso.table.data.column.storage.datetime.TimeOfDayStorage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.TextType;
import org.graalvm.polyglot.Context;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.function.Function;

public class ToTextStorageConverter implements StorageConverter<String> {
  private final TextType targetType;

  public ToTextStorageConverter(TextType textType) {
    targetType = textType;
  }

  public Storage<String> cast(Storage<?> storage, CastProblemBuilder problemBuilder) {
    if (storage instanceof StringStorage stringStorage) {
      if (canAvoidCopying(stringStorage)) {
        return retypeStringStorage(stringStorage);
      } else {
        return adaptStringStorage(stringStorage, problemBuilder);
      }
    }
    if (storage instanceof AbstractLongStorage longStorage) {
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
      throw new IllegalStateException("No known strategy for casting storage " + storage + " to Text.");
    }
  }

  public Storage<String> castFromMixed(Storage<?> mixedStorage, CastProblemBuilder problemBuilder) {
    Context context = Context.getCurrent();
    StringBuilder builder = new StringBuilder(mixedStorage.size(), targetType);
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

      context.safepoint();
    }

    problemBuilder.aggregateOtherProblems(builder.getProblems());
    return builder.seal();
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

  private Storage<String> castLongStorage(AbstractLongStorage longStorage, CastProblemBuilder problemBuilder) {
    Context context = Context.getCurrent();
    StringBuilder builder = new StringBuilder(longStorage.size(), targetType);
    for (int i = 0; i < longStorage.size(); i++) {
      if (longStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        long value = longStorage.getItem(i);
        builder.append(adapt(Long.toString(value), problemBuilder));
      }

      context.safepoint();
    }

    problemBuilder.aggregateOtherProblems(builder.getProblems());
    return builder.seal();
  }

  private Storage<String> castBoolStorage(BoolStorage boolStorage, CastProblemBuilder problemBuilder) {
    Context context = Context.getCurrent();
    StringBuilder builder = new StringBuilder(boolStorage.size(), targetType);
    for (int i = 0; i < boolStorage.size(); i++) {
      if (boolStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        boolean value = boolStorage.getItem(i);
        builder.append(adapt(convertBoolean(value), problemBuilder));
      }

      context.safepoint();
    }

    problemBuilder.aggregateOtherProblems(builder.getProblems());
    return builder.seal();
  }

  private Storage<String> castDoubleStorage(DoubleStorage doubleStorage, CastProblemBuilder problemBuilder) {
    Context context = Context.getCurrent();
    StringBuilder builder = new StringBuilder(doubleStorage.size(), targetType);
    for (int i = 0; i < doubleStorage.size(); i++) {
      if (doubleStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        double value = doubleStorage.getItemAsDouble(i);
        builder.append(adapt(Double.toString(value), problemBuilder));
      }

      context.safepoint();
    }

    problemBuilder.aggregateOtherProblems(builder.getProblems());
    return builder.seal();
  }

  private <T> Storage<String> castDateTimeStorage(Storage<T> storage, Function<T, String> converter,
                                                  CastProblemBuilder problemBuilder) {
    Context context = Context.getCurrent();
    StringBuilder builder = new StringBuilder(storage.size(), targetType);
    for (int i = 0; i < storage.size(); i++) {
      if (storage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        T value = storage.getItemBoxed(i);
        String converted = converter.apply(value);
        builder.append(adapt(converted, problemBuilder));
      }

      context.safepoint();
    }

    problemBuilder.aggregateOtherProblems(builder.getProblems());
    return builder.seal();
  }

  private String adapt(String value, CastProblemBuilder problemBuilder) {
    String adapted = adaptWithoutWarning(value);

    // If the value was truncated, report the data loss.
    // (We can use the codepoint lengths here because truncation on grapheme length will still change the codepoint
    // length too, and this check is simply faster.)
    if (adapted.length() < value.length()) {
      problemBuilder.reportTextTooLong(value);
    }

    return adapted;
  }

  private String adaptWithoutWarning(String value) {
    return targetType.adapt(value);
  }

  private Storage<String> adaptStringStorage(StringStorage stringStorage, CastProblemBuilder problemBuilder) {
    Context context = Context.getCurrent();
    StringBuilder builder = new StringBuilder(stringStorage.size(), targetType);
    for (int i = 0; i < stringStorage.size(); i++) {
      if (stringStorage.isNa(i)) {
        builder.appendNulls(1);
      } else {
        String value = stringStorage.getItem(i);
        // Adapting an existing string storage into a new type is done without warnings.
        builder.append(adaptWithoutWarning(value));
      }

      context.safepoint();
    }

    problemBuilder.aggregateOtherProblems(builder.getProblems());
    return builder.seal();
  }

  private boolean canAvoidCopying(StringStorage stringStorage) {
    if (targetType.fitsExactly(stringStorage.getType())) {
      return true;
    }

    long maxLength = Long.MIN_VALUE;
    long minLength = Long.MAX_VALUE;
    for (int i = 0; i < stringStorage.size(); i++) {
      String value = stringStorage.getItem(i);
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
   * <p>
   * This can only be done if the values do not need any adaptations, checked by {@code canAvoidCopying}.
   */
  private Storage<String> retypeStringStorage(StringStorage stringStorage) {
    return new StringStorage(stringStorage.getData(), stringStorage.size(), targetType);
  }
}
