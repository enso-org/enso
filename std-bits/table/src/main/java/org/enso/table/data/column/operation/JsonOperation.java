package org.enso.table.data.column.operation;

import static java.time.temporal.ChronoField.*;
import static java.time.temporal.ChronoField.NANO_OF_SECOND;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.enso.base.polyglot.EnsoMeta;
import org.enso.table.data.column.DataQualityMetrics;
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
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;
import org.enso.table.util.LeastRecentlyUsedCache;
import org.graalvm.polyglot.Context;
import org.slf4j.Logger;

/**
 * A utility class for converting column data to JSON format. This is used for visualization
 * purposes.
 */
public class JsonOperation {
  private static final Logger LOGGER = org.slf4j.LoggerFactory.getLogger(JsonOperation.class);

  private static Function<Object, String> _ensoJsonCallback;

  private static Function<Object, String> ensoJsonCallback() {
    if (_ensoJsonCallback != null) {
      return _ensoJsonCallback;
    }

    try {
      var jsonType = EnsoMeta.getType("Standard.Visualization.Table.Visualization", "Helper");
      var method = jsonType.getMember("make_json");
      LOGGER.info("Resolved Enso JSON callback: {}", method);
      _ensoJsonCallback =
          value -> {
            LOGGER.info(
                "Calling Enso JSON callback for value: {} (class {})",
                value,
                value == null ? "null" : value.getClass());
            var result = method.execute(jsonType, value);
            return result == null || result.isNull() ? "null" : result.asString();
          };
      return _ensoJsonCallback;
    } catch (Exception ex) {
      LOGGER.warn("Failed to resolve Enso JSON callback.", ex);
      return null;
    }
  }

  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

  private record CacheKey(long storageKey, long start, long length) {}

  private static LeastRecentlyUsedCache<CacheKey, String> _jsonCache;

  private static LeastRecentlyUsedCache<CacheKey, String> jsonCache() {
    if (_jsonCache == null) {
      _jsonCache = new LeastRecentlyUsedCache<>(1000);
    }
    return _jsonCache;
  }

  public static String apply(Column source, long start, long maxLength) {
    var fullStorage = ColumnStorageWithInferredStorage.resolveStorage(source);
    var cacheKey = new CacheKey(fullStorage.uniqueKey(), start, maxLength);
    final long finalLength = maxLength;
    return jsonCache().computeIfAbsent(cacheKey, _ -> applyImpl(start, fullStorage, finalLength));
  }

  private static String applyImpl(long start, ColumnStorage<?> fullStorage, long finalLength) {
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

  private static String createObjectJson(ColumnStorage<?> storage, long start, long length) {
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

  public static String objectToJson(Object value) {
    return objectToJson(value, ensoJsonCallback());
  }

  public static String objectToJson(Object value, Function<Object, String> ensoJsonCallback) {
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
      case LocalDate date -> toJson(date);
      case LocalTime time -> toJson(time);
      case ZonedDateTime zdt -> toJson(zdt);
      default -> {
        if (ensoJsonCallback == null) {
          LOGGER.debug("Could not serialize value of type {}.", value.getClass());
          yield "null";
        } else {
          yield ensoJsonCallback.apply(value);
        }
      }
    };
  }

  private static String toJson(boolean value) {
    return value ? "true" : "false";
  }

  private static final long MAX_JSON_LONG = 9007199254740991L;
  private static final BigInteger MAX_JSON_LONG_BIGINT = BigInteger.valueOf(MAX_JSON_LONG);

  private static final DateTimeFormatter TIME_SHORT_FORMAT =
      DateTimeFormatter.ofPattern("HH:mm:ss");
  private static final DateTimeFormatter TIME_LONG_FORMAT =
      DateTimeFormatter.ofPattern("HH:mm:ss.SSSSSS");
  private static final DateTimeFormatter DATE_TIME_SHORT_FORMAT =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
  private static final DateTimeFormatter DATE_TIME_LONG_FORMAT =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.ggg");
  private static final DateTimeFormatter ZONE_FORMAT = DateTimeFormatter.ofPattern("'['zz']'");

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

  private static LeastRecentlyUsedCache<String, String> _tableVizCache;

  private static LeastRecentlyUsedCache<String, String> tableVizCache() {
    if (_tableVizCache == null) {
      _tableVizCache = new LeastRecentlyUsedCache<>(1000);
    }
    return _tableVizCache;
  }

  private static final int MAX_CELLS_FOR_INLINE = 2500;

  /**
   * Creates a JSON string representing the table visualization metadata, including column headers,
   * value types, and various properties related to the table's structure and behavior.
   */
  public static String makeTableVizJSON(
      String versionId,
      Column[] columns,
      long allRowsCount,
      boolean useServerMode,
      List<String> valueTypeDisplay,
      String getChildMethod) {
    if (allRowsCount == -1) {
      final boolean finalUseServerMode = useServerMode;

      if (tableVizCache().containsKey(versionId)) {
        return tableVizCache().get(versionId);
      }

      var incomplete =
          Arrays.stream(columns)
              .anyMatch(
                  c -> DataQualityMetrics.get(c).get(DataQualityMetrics.IS_INCOMPLETE) != null);
      if (incomplete) {
        LOGGER.info("Table version {} is incomplete, skipping cache", versionId);
        allRowsCount = columns[0].getSize();
      } else {
        LOGGER.info("Table version {} generating JSON", versionId);
        return tableVizCache()
            .computeIfAbsent(
                versionId,
                _ ->
                    makeTableVizJSON(
                        versionId,
                        columns,
                        columns[0].getSize(),
                        finalUseServerMode,
                        valueTypeDisplay,
                        getChildMethod));
      }
    }

    boolean isColumn = !"get_row".equals(getChildMethod);
    boolean isDBMode = !useServerMode;
    boolean useServer = useServerMode && (allRowsCount * columns.length) > MAX_CELLS_FOR_INLINE;

    var jsonBuilder = new StringBuilder();
    jsonBuilder.append("{");

    var headers = new StringBuilder();
    var valueTypes = new StringBuilder();
    var metrics = new ArrayList<Map<String, Object>>(columns.length);
    for (int i = 0; i < columns.length; i++) {
      if (!headers.isEmpty()) {
        headers.append(",");
        valueTypes.append(",");
      }
      headers.append(toJson(columns[i].getName()));

      var columnType = columns[i].getStorageType().ensoConstructorName();
      valueTypes
          .append("{\"constructor\":\"")
          .append(columnType)
          .append("\",\"display_text\":\"")
          .append(valueTypeDisplay.get(i))
          .append("\"}");

      metrics.add(isDBMode ? Map.of() : DataQualityMetrics.get(columns[i]));
    }
    jsonBuilder.append("\"header\":[").append(headers).append("]");
    jsonBuilder.append(",\"value_type\":[").append(valueTypes).append("]");

    appendProperty(jsonBuilder, "all_rows_count", allRowsCount);
    appendProperty(jsonBuilder, "has_index_col", true);
    appendProperty(jsonBuilder, "get_child_node_action", getChildMethod);
    appendProperty(jsonBuilder, "use_bottom_status_bar", !isDBMode);
    appendProperty(jsonBuilder, "enable_create_node", !isColumn);

    jsonBuilder.append(",\"data_quality_metrics\":[");
    makeDataQualityMetrics(jsonBuilder, metrics);
    jsonBuilder.append("]");

    appendProperty(jsonBuilder, "type", "EnsoTableOrColumn");
    appendProperty(jsonBuilder, "child_label", "row");
    appendProperty(jsonBuilder, "is_using_server_sort_and_filter", useServer);
    appendMetric(
        jsonBuilder, "requires_number_format", metrics, DataQualityMetrics.NEEDS_FORMATTING, false);
    appendProperty(jsonBuilder, "table_version_hash", isDBMode ? null : versionId);
    appendMetric(
        jsonBuilder, "is_using_multi_filter", metrics, DataQualityMetrics.USE_MULTI_FILTER, false);
    jsonBuilder.append(",\"data\":").append(useServer ? "null" : dataToJson(columns));

    jsonBuilder.append("}");
    return jsonBuilder.toString();
  }

  private static void appendProperty(StringBuilder builder, String name, Object value) {
    if (builder.length() > 1) {
      builder.append(",");
    }
    builder.append("\"").append(name).append("\":").append(objectToJson(value));
  }

  private static void makeDataQualityMetrics(StringBuilder json, List<Map<String, Object>> dqs) {
    boolean f = true;
    f = addMetric(json, dqs, "", DataQualityMetrics.IS_INCOMPLETE_TEXT, "Text", f, "");
    f = addRange(json, dqs, f);
    f =
        addMetric(
            json, dqs, "Number of distinct", DataQualityMetrics.DISTINCT_COUNT, "Count", f, 0);
    f = addMetric(json, dqs, "% nothing", DataQualityMetrics.NOTHING_COUNT, "Percentage", f, null);
    f = addMetric(json, dqs, "", DataQualityMetrics.TYPE_RECORD, "Text", f, null);

    var sampled =
        dqs.stream().anyMatch(m -> Boolean.TRUE.equals(m.get(DataQualityMetrics.SAMPLED)));
    var suffix = sampled ? " (sampled)" : "";

    f =
        addMetric(
            json, dqs, "% empty" + suffix, DataQualityMetrics.EMPTY_COUNT, "Percentage", f, null);
    f =
        addMetric(
            json,
            dqs,
            "% untrimmed" + suffix,
            DataQualityMetrics.UNTRIMMED_COUNT,
            "Percentage",
            f,
            null);
    f =
        addMetric(
            json,
            dqs,
            "% with odd whitespace" + suffix,
            DataQualityMetrics.ODD_SPACE_COUNT,
            "Percentage",
            f,
            null);
  }

  private static boolean addMetric(
      StringBuilder builder,
      List<Map<String, Object>> metrics,
      String label,
      String fieldName,
      String type,
      boolean first,
      Object defaultValue) {
    if (!hasMetric(metrics, fieldName)) {
      return first;
    }

    if (!first) {
      builder.append(",");
    }

    builder.append("{\"name\":\"").append(label).append("\"");
    appendMetric(builder, "values", metrics, fieldName, defaultValue);
    builder.append(",\"type\":\"").append(type).append("\"}");
    return false;
  }

  public static boolean addRange(
      StringBuilder builder, List<Map<String, Object>> metrics, boolean first) {
    boolean hasRange = false;
    List<String> ranges = new ArrayList<>();

    for (var metric : metrics) {
      var min = metric.get(DataQualityMetrics.MINIMUM);
      if (min == null) {
        ranges.add(null);
        continue;
      }

      hasRange = true;
      String rangeValue =
          Boolean.TRUE.equals(metric.get(DataQualityMetrics.SINGLE_VALUE))
              ? toDisplayText(min)
              : toDisplayText(min) + " - " + toDisplayText(metric.get(DataQualityMetrics.MAXIMUM));
      ranges.add(objectToJson(rangeValue));
    }

    if (!hasRange) {
      return first;
    }

    if (!first) {
      builder.append(",");
    }
    builder
        .append("{\"name\":\"Range\",\"values\":[")
        .append(String.join(",", ranges))
        .append("],\"type\":\"Text\"}");
    return false;
  }

  private static final DateTimeFormatter DATE_TIME_FORMATTER =
      new DateTimeFormatterBuilder()
          .appendValue(YEAR, 4)
          .appendLiteral('-')
          .appendValue(MONTH_OF_YEAR, 2)
          .appendLiteral('-')
          .appendValue(DAY_OF_MONTH, 2)
          .appendLiteral(' ')
          .appendValue(HOUR_OF_DAY, 2)
          .appendLiteral(':')
          .appendValue(MINUTE_OF_HOUR, 2)
          .appendLiteral(':')
          .appendValue(SECOND_OF_MINUTE, 2)
          .optionalStart()
          .appendFraction(NANO_OF_SECOND, 0, 3, true)
          .optionalStart()
          .appendLiteral('[')
          .appendZoneRegionId()
          .appendLiteral(']')
          .toFormatter();

  private static final DateTimeFormatter TIME_FORMATTER =
      new DateTimeFormatterBuilder()
          .appendValue(HOUR_OF_DAY, 2)
          .appendLiteral(':')
          .appendValue(MINUTE_OF_HOUR, 2)
          .appendLiteral(':')
          .appendValue(SECOND_OF_MINUTE, 2)
          .optionalStart()
          .appendFraction(NANO_OF_SECOND, 0, 6, true)
          .toFormatter();

  private static String toDisplayText(Object value) {
    if (value instanceof LocalTime localTime) {
      return localTime.format(TIME_FORMATTER);
    } else if (value instanceof ZonedDateTime zonedDateTime) {
      return zonedDateTime.getZone() == ZoneId.systemDefault()
          ? zonedDateTime.toLocalDateTime().format(DATE_TIME_FORMATTER)
          : zonedDateTime.format(DATE_TIME_FORMATTER);
    } else {
      return value.toString();
    }
  }

  private static boolean hasMetric(List<Map<String, Object>> metrics, String metric) {
    return metrics.stream().anyMatch(dqm -> dqm.get(metric) != null);
  }

  private static void appendMetric(
      StringBuilder builder,
      String name,
      List<Map<String, Object>> metrics,
      String metric,
      Object defaultValue) {
    if (builder.length() > 1) {
      builder.append(",");
    }
    builder.append("\"").append(name).append("\":[");
    for (int i = 0; i < metrics.size(); i++) {
      if (i != 0) {
        builder.append(",");
      }
      builder.append(objectToJson(metrics.get(i).getOrDefault(metric, defaultValue)));
    }
    builder.append("]");
  }

  private static String dataToJson(Column[] columns) {
    var output = new ArrayList<String>();
    for (Column column : columns) {
      output.add(apply(column, 0, column.getSize()));
    }
    return output.stream().collect(Collectors.joining(",", "[", "]"));
  }
}
