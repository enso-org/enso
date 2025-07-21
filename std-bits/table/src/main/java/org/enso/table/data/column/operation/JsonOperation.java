package org.enso.table.data.column.operation;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.function.Function;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.table.Column;
import org.graalvm.polyglot.Context;

public class JsonOperation {
  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

  public static String apply(Column source, Function<Object, String> ensoJsonCallback) {
    var storage = ColumnStorageWithInferredStorage.resolveStorage(source);

    return switch (storage.getType()) {
      case NullType nullType -> createNullJson(storage.getSize());
      case BooleanType booleanType -> createBooleanJson(booleanType.asTypedStorage(storage));
      case IntegerType integerType -> createIntegerJson(integerType.asTypedStorage(storage));
      case FloatType floatType -> createFloatJson(floatType.asTypedStorage(storage));
      default -> createObjectJson(storage, ensoJsonCallback);
    };
  }

  private static String createFloatJson(ColumnDoubleStorage doubleStorage) {
    long size = doubleStorage.getSize();
    var context = Context.getCurrent();
    StringBuilder builder = new StringBuilder();
    builder.append("[");
    for (long i = 0; i < size; i++) {
      if (i > 0) {
        builder.append(",");
      }
      builder.append(
          doubleStorage.isNothing(i) ? "null" : toJson(doubleStorage.getItemAsDouble(i)));
      context.safepoint();
    }
    builder.append("]");
    return builder.toString();
  }

  private static String createIntegerJson(ColumnLongStorage longStorage) {
    long size = longStorage.getSize();
    var context = Context.getCurrent();
    StringBuilder builder = new StringBuilder();
    builder.append("[");
    for (long i = 0; i < size; i++) {
      if (i > 0) {
        builder.append(",");
      }
      builder.append(longStorage.isNothing(i) ? "null" : toJson(longStorage.getItemAsLong(i)));
      context.safepoint();
    }
    builder.append("]");
    return builder.toString();
  }

  private static String createBooleanJson(ColumnBooleanStorage booleanStorage) {
    long size = booleanStorage.getSize();
    var context = Context.getCurrent();
    StringBuilder builder = new StringBuilder();
    builder.append("[");
    for (long i = 0; i < size; i++) {
      if (i > 0) {
        builder.append(",");
      }
      builder.append(
          booleanStorage.isNothing(i) ? "null" : toJson(booleanStorage.getItemAsBoolean(i)));
      context.safepoint();
    }
    builder.append("]");
    return builder.toString();
  }

  private static String createObjectJson(
      ColumnStorage<?> storage, Function<Object, String> ensoJsonCallback) {
    long size = storage.getSize();
    var context = Context.getCurrent();
    StringBuilder builder = new StringBuilder();
    builder.append("[");
    for (long i = 0; i < size; i++) {
      if (i > 0) {
        builder.append(",");
      }

      Object value = storage.getItemBoxed(i);
      switch (value) {
        case null -> builder.append("null");
        case Boolean b -> builder.append(toJson(b));
        case Long l -> builder.append(toJson(l));
        case Double d -> builder.append(toJson(d));
        case String s -> builder.append(toJson(s));
        case BigInteger bi -> builder.append(toJson(bi));
        case BigDecimal bd -> builder.append(toJson(bd));
        case LocalDate date -> builder.append(toJson(date));
        case LocalTime time -> builder.append(toJson(time));
        case ZonedDateTime zdt -> builder.append(toJson(zdt));
        default -> builder.append(ensoJsonCallback.apply(value));
      }
      context.safepoint();
    }
    builder.append("]");
    return builder.toString();
  }

  private static String createNullJson(long size) {
    int checkedSize = Builder.checkSize(size);
    return checkedSize == 0
        ? "[]"
        : "[" + String.join(",", Collections.nCopies(checkedSize, "null")) + "]";
  }

  private static String toJson(boolean value) {
    return value ? "true" : "false";
  }

  private static long MAX_JSON_LONG = 9007199254740991L;
  private static BigInteger MAX_JSON_LONG_BIGINT = BigInteger.valueOf(MAX_JSON_LONG);

  private static DateTimeFormatter TIME_SHORT_FORMAT = DateTimeFormatter.ofPattern("HH:mm:ss");
  private static DateTimeFormatter TIME_LONG_FORMAT =
      DateTimeFormatter.ofPattern("HH:mm:ss.SSSSSS");
  private static DateTimeFormatter DATE_TIME_SHORT_FORMAT =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
  private static DateTimeFormatter DATE_TIME_LONG_FORMAT =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.ggg");
  private static DateTimeFormatter ZONE_FORMAT = DateTimeFormatter.ofPattern("'['zz']'");

  private static String toJson(long value) {
    if (value < -MAX_JSON_LONG || value > MAX_JSON_LONG) {
      return "{\"type\":\"Integer\",\"value\":\"" + value + "\"}";
    }
    return String.valueOf(value);
  }

  private static String toJson(double value) {
    return String.valueOf(value);
  }

  private static String toJson(BigInteger value) {
    if (value.abs().compareTo(MAX_JSON_LONG_BIGINT) > 0) {
      return "{\"type\":\"Integer\",\"value\":\"" + value + "\"}";
    }
    return value.toString();
  }

  private static String toJson(BigDecimal value) {
    return "{\"type\":\"Decimal\",\"value\":\""
        + value
        + "\",\"scale\":"
        + value.scale()
        + ",\"precision\":"
        + value.precision()
        + "}";
  }

  private static String toJson(String value) {
    try {
      return OBJECT_MAPPER.writeValueAsString(value);
    } catch (JsonProcessingException e) {
      throw new RuntimeException(e);
    }
  }

  private static String toJson(LocalDate date) {
    return "{\"type\":\"Date\",\"constructor\":\"new\",\"_display_text_\":\""
        + date.toString()
        + "\",\"day\":"
        + date.getDayOfMonth()
        + ",\"month\":"
        + date.getMonthValue()
        + ",\"year\":"
        + date.getYear()
        + "}";
  }

  private static String toJson(LocalTime time) {
    var timeString = time.format(time.getNano() == 0 ? TIME_SHORT_FORMAT : TIME_LONG_FORMAT);
    return "{\"type\":\"Time_Of_Day\",\"constructor\":\"new\",\"_display_text_\":\""
        + timeString
        + "\",\"hour\":"
        + time.getHour()
        + ",\"minute\":"
        + time.getMinute()
        + ",\"second\":"
        + time.getSecond()
        + ",\"nanosecond\":"
        + time.getNano()
        + "}";
  }

  private static String toJson(ZonedDateTime datetime) {
    var datetimeString =
        datetime.format(datetime.getNano() == 0 ? DATE_TIME_SHORT_FORMAT : DATE_TIME_LONG_FORMAT);
    var zoneString =
        datetime.getZone() == ZoneId.systemDefault() ? "" : datetime.format(ZONE_FORMAT);
    var zone_json =
        "{\"type\":\"Time_Zone\",\"constructor\":\"parse\",\"id\":\""
            + datetime.getZone().getId()
            + "\"}";
    return "{\"type\":\"Date_Time\",\"constructor\":\"new\",\"_display_text_\":\""
        + datetimeString
        + zoneString
        + "\",\"year\":"
        + datetime.getYear()
        + ",\"month\":"
        + datetime.getMonthValue()
        + ",\"day\":"
        + datetime.getDayOfMonth()
        + ",\"hour\":"
        + datetime.getHour()
        + ",\"minute\":"
        + datetime.getMinute()
        + ",\"second\":"
        + datetime.getSecond()
        + ",\"nanosecond\":"
        + datetime.getNano()
        + ",\"zone\":"
        + zone_json
        + "}";
  }
}
