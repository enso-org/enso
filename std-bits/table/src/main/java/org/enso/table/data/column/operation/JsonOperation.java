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
import org.enso.base.polyglot.EnsoMeta;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.type.*;
import org.enso.table.data.table.Column;
import org.enso.table.util.LeastRecentlyUsedCache;
import org.graalvm.polyglot.Context;
import org.slf4j.Logger;

/** A utility class for converting column data to JSON format. */
public class JsonOperation {
  private static final Logger LOGGER = org.slf4j.LoggerFactory.getLogger(JsonOperation.class);
  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

  private record CacheKey(long storageKey, long start, long length) {}

  public static JsonOperation INSTANCE =
      new JsonOperation("Standard.Base.Data.Json", "Json", "stringify", false);
  public static JsonOperation VIZ_INSTANCE =
      new JsonOperation("Standard.Visualization.Table.Visualization", "Helper", "make_json", true);

  private final String ensoCallbackModule;
  private final String ensoCallbackType;
  private final String ensoCallbackMethod;
  private final boolean includeDisplayText;
  private final LeastRecentlyUsedCache<CacheKey, String> jsonCache;
  private Function<Object, String> _ensoCallback;

  private JsonOperation(
      String ensoCallbackModule,
      String ensoCallbackType,
      String ensoCallbackMethod,
      boolean includeDisplayText) {
    this.ensoCallbackModule = ensoCallbackModule;
    this.ensoCallbackType = ensoCallbackType;
    this.ensoCallbackMethod = ensoCallbackMethod;
    this.includeDisplayText = includeDisplayText;
    jsonCache = new LeastRecentlyUsedCache<>(1000);
  }

  private Function<Object, String> ensoCallback() {
    if (_ensoCallback != null) {
      return _ensoCallback;
    }

    try {
      var jsonType = EnsoMeta.getType(ensoCallbackModule, ensoCallbackType);
      var method = jsonType.getMember(ensoCallbackMethod);
      LOGGER.info("Resolved Enso JSON callback: {}", method);
      _ensoCallback =
          value -> {
            LOGGER.debug(
                "Calling Enso JSON callback for value: {} (class {})",
                value,
                value == null ? "null" : value.getClass());
            var result = method.execute(jsonType, value);
            return result == null || result.isNull() ? "null" : result.asString();
          };
      return _ensoCallback;
    } catch (Exception ex) {
      LOGGER.warn("Failed to resolve Enso JSON callback.", ex);
      return null;
    }
  }

  /**
   * Create a JSON serialized column from an input Column. The resulting column will contain JSON
   * strings representing the values in the source column. The method will attempt to use native
   * JSON serialization for supported types, and will fall back to Enso `Json.stringify` when
   * needed.
   *
   * @param source column to serialize as JSON.
   * @return a new Column of the values serialized as JSON.
   */
  public Column makeJsonColumn(Column source) {
    var fullStorage = ColumnStorageWithInferredStorage.resolveStorage(source);
    return makeJsonColumFromStorage(source.getName(), fullStorage);
  }

  private Column makeJsonColumFromStorage(String columnName, ColumnStorage<?> storage) {
    var result =
        StorageIterators.buildObjectOverStorage(
            storage,
            false,
            Builder.getForText(TextType.VARIABLE_LENGTH, storage.getSize()),
            (builder, _, value) -> builder.append(objectToJson(value)));
    return new Column(columnName, result);
  }

  public String apply(Column source, long start, long maxLength) {
    var fullStorage = ColumnStorageWithInferredStorage.resolveStorage(source);
    var cacheKey = new CacheKey(fullStorage.uniqueKey(), start, maxLength);
    final long finalLength = maxLength;
    return jsonCache.computeIfAbsent(cacheKey, _ -> applyImpl(start, fullStorage, finalLength));
  }

  private String applyImpl(long start, ColumnStorage<?> fullStorage, long finalLength) {
    if (start >= fullStorage.getSize()) {
      // If the start is beyond the size of the storage, return an empty array.
      return "[]";
    }
    long length = finalLength;
    if (start + length > fullStorage.getSize()) {
      // If the requested length goes beyond the size of the storage, adjust it.
      length = fullStorage.getSize() - start;
    }

    return switch (StorageType.ofStorage(fullStorage)) {
      case NullType _ -> createNullJson(length);
      case BooleanType booleanType ->
          createBooleanJson(booleanType.asTypedStorage(fullStorage), start, length);
      case IntegerType integerType ->
          createIntegerJson(integerType.asTypedStorage(fullStorage), start, length);
      case FloatType floatType ->
          createFloatJson(floatType.asTypedStorage(fullStorage), start, length);
      default -> createObjectJson(fullStorage, start, length);
    };
  }

  public String objectToJson(Object value) {
    return switch (value) {
      case null -> "null";
      case Boolean b -> toJson(b);
      case Long l -> toJson(l);
      case Integer i -> toJson(i);
      case Short s -> toJson(s);
      case Byte b -> toJson(b & 0xFF);
      case Double d -> toJson(d);
      case Float f -> toJson(f);
      case String s -> toJson(s);
      case BigInteger bi -> toJson(bi);
      case BigDecimal bd -> toJson(bd);
      case LocalDate date -> toJson(date, includeDisplayText);
      case LocalTime time -> toJson(time, includeDisplayText);
      case ZonedDateTime zdt -> toJson(zdt, includeDisplayText);
      default -> {
        var callback = ensoCallback();
        if (callback == null) {
          LOGGER.info("Could not serialize value of type {}.", value.getClass());
          yield "null";
        } else {
          yield callback.apply(value);
        }
      }
    };
  }

  /**
   * Check if a value is natively supported by the JSON Operation
   *
   * @param value to check
   * @return true if the value is natively supported, false otherwise
   */
  public static boolean nativeSupport(Object value) {
    return switch (value) {
      case null -> true;
      case Boolean _,
          Long _,
          Integer _,
          Short _,
          Byte _,
          Double _,
          Float _,
          String _,
          BigInteger _,
          BigDecimal _,
          LocalDate _,
          LocalTime _,
          ZonedDateTime _ ->
          true;
      default -> false;
    };
  }

  private static String createFloatJson(
      ColumnDoubleStorage doubleStorage, long start, long length) {
    var context = Context.getCurrent();
    StringBuilder builder = new StringBuilder();
    builder.append("[");
    for (long i = start; i < (start + length); i++) {
      if (i > start) {
        builder.append(",");
      }
      builder.append(
          doubleStorage.isNothing(i) ? "null" : toJson(doubleStorage.getItemAsDouble(i)));
      context.safepoint();
    }
    builder.append("]");
    return builder.toString();
  }

  private static String createIntegerJson(ColumnLongStorage longStorage, long start, long length) {
    var context = Context.getCurrent();
    StringBuilder builder = new StringBuilder();
    builder.append("[");
    for (long i = start; i < (start + length); i++) {
      if (i > start) {
        builder.append(",");
      }
      builder.append(longStorage.isNothing(i) ? "null" : toJson(longStorage.getItemAsLong(i)));
      context.safepoint();
    }
    builder.append("]");
    return builder.toString();
  }

  private static String createBooleanJson(
      ColumnBooleanStorage booleanStorage, long start, long length) {
    var context = Context.getCurrent();
    StringBuilder builder = new StringBuilder();
    builder.append("[");
    for (long i = start; i < (start + length); i++) {
      if (i > start) {
        builder.append(",");
      }
      builder.append(
          booleanStorage.isNothing(i) ? "null" : toJson(booleanStorage.getItemAsBoolean(i)));
      context.safepoint();
    }
    builder.append("]");
    return builder.toString();
  }

  private String createObjectJson(ColumnStorage<?> storage, long start, long length) {
    var context = Context.getCurrent();
    StringBuilder builder = new StringBuilder();
    builder.append("[");
    for (long i = start; i < (start + length); i++) {
      if (i > start) {
        builder.append(",");
      }

      Object value = storage.getItemBoxed(i);
      String jsonValue = objectToJson(value);
      builder.append(jsonValue);
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

  private static final long MAX_JSON_LONG = 9007199254740991L;
  private static final BigInteger MAX_JSON_LONG_BIGINT = BigInteger.valueOf(MAX_JSON_LONG);

  private static String toJson(long value) {
    if (value < -MAX_JSON_LONG || value > MAX_JSON_LONG) {
      return "{\"type\":\"Integer\",\"value\":\"" + value + "\"}";
    }
    return String.valueOf(value);
  }

  private static String toJson(double value) {
    if (Double.isNaN(value)) {
      return "{\"_display_text_\":\"NaN\",\"type\":\"Float\",\"value\":\"NaN\"}";
    }
    if (Double.isInfinite(value)) {
      var txtValue = value > 0 ? "Infinity" : "-Infinity";
      return "{\"_display_text_\":\""
          + txtValue
          + "\",\"type\":\"Float\",\"value\":\""
          + txtValue
          + "\"}";
    }
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

  private static String toJson(LocalDate date, boolean includeDisplayText) {
    String displayText =
        includeDisplayText
            ? "\"_display_text_\":\"" + date.toString() + "\","
            : "";
    return "{\"type\":\"Date\",\"constructor\":\"new\","
        + displayText
        + "\"day\":"
        + date.getDayOfMonth()
        + ",\"month\":"
        + date.getMonthValue()
        + ",\"year\":"
        + date.getYear()
        + "}";
  }

  private static final DateTimeFormatter TIME_SHORT_FORMAT =
      DateTimeFormatter.ofPattern("HH:mm:ss");
  private static final DateTimeFormatter TIME_LONG_FORMAT =
      DateTimeFormatter.ofPattern("HH:mm:ss.SSSSSS");

  private static String toJson(LocalTime time, boolean includeDisplayText) {
    var timeString =
        includeDisplayText
            ? time.format(time.getNano() == 0 ? TIME_SHORT_FORMAT : TIME_LONG_FORMAT)
            : "";
    String displayText =
        includeDisplayText
            ? "\"_display_text_\":\"" + timeString + "\","
            : "";
    return "{\"type\":\"Time_Of_Day\",\"constructor\":\"new\","
        + displayText
        + "\"hour\":"
        + time.getHour()
        + ",\"minute\":"
        + time.getMinute()
        + ",\"second\":"
        + time.getSecond()
        + ",\"nanosecond\":"
        + time.getNano()
        + "}";
  }

  private static final DateTimeFormatter DATE_TIME_SHORT_FORMAT =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
  private static final DateTimeFormatter DATE_TIME_LONG_FORMAT =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS");
  private static final DateTimeFormatter ZONE_FORMAT = DateTimeFormatter.ofPattern("'['zz']'");

  private static String toJson(ZonedDateTime datetime, boolean includeDisplayText) {
    var datetimeString =
        includeDisplayText
            ? datetime.format(
                datetime.getNano() == 0 ? DATE_TIME_SHORT_FORMAT : DATE_TIME_LONG_FORMAT)
            : "";
    var zoneString =
        includeDisplayText && datetime.getZone() != ZoneId.systemDefault()
            ? datetime.format(ZONE_FORMAT)
            : "";
    String displayText =
        includeDisplayText
            ? "\"_display_text_\":\"" + datetimeString + zoneString + "\","
            : "";
    var zone_json =
        "{\"type\":\"Time_Zone\",\"constructor\":\"parse\",\"id\":\""
            + datetime.getZone().getId()
            + "\"}";
    return "{\"type\":\"Date_Time\",\"constructor\":\"new\","
        + displayText
        + "\"year\":"
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
