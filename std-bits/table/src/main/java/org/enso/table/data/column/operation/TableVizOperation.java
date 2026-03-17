package org.enso.table.data.column.operation;

import org.enso.table.data.column.DataQualityMetrics;
import org.enso.table.data.table.Column;
import org.enso.table.util.LeastRecentlyUsedCache;
import org.slf4j.Logger;

import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static java.time.temporal.ChronoField.DAY_OF_MONTH;
import static java.time.temporal.ChronoField.HOUR_OF_DAY;
import static java.time.temporal.ChronoField.MINUTE_OF_HOUR;
import static java.time.temporal.ChronoField.MONTH_OF_YEAR;
import static java.time.temporal.ChronoField.NANO_OF_SECOND;
import static java.time.temporal.ChronoField.SECOND_OF_MINUTE;
import static java.time.temporal.ChronoField.YEAR;

/**
 * Extension to JsonOperation for TableViz JSON code.
 */
public class TableVizOperation {
  private static final Logger LOGGER = org.slf4j.LoggerFactory.getLogger(JsonOperation.class);

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
  public static String makeJSON(
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
                    makeJSON(
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
      headers.append(JsonOperation.VIZ_INSTANCE.objectToJson(columns[i].getName()));

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
    builder.append("\"").append(name).append("\":").append(JsonOperation.VIZ_INSTANCE.objectToJson(value));
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

  private static boolean addRange(
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
      ranges.add(JsonOperation.VIZ_INSTANCE.objectToJson(rangeValue));
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
      builder.append(JsonOperation.VIZ_INSTANCE.objectToJson(metrics.get(i).getOrDefault(metric, defaultValue)));
    }
    builder.append("]");
  }

  private static String dataToJson(Column[] columns) {
    var output = new ArrayList<String>();
    for (Column column : columns) {
      output.add(JsonOperation.VIZ_INSTANCE.apply(column, 0, column.getSize()));
    }
    return output.stream().collect(Collectors.joining(",", "[", "]"));
  }
}
