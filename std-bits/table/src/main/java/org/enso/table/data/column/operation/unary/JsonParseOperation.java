package org.enso.table.data.column.operation.unary;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.function.Function;
import org.enso.base.polyglot.EnsoMeta;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;
import org.graalvm.polyglot.Value;

/** Deserializes JSON to Objects in the Table. */
public class JsonParseOperation implements UnaryOperation {
  @Override
  public String getName() {
    return "parse_json";
  }

  @Override
  public boolean canApply(ColumnStorage<?> storage) {
    return StorageType.ofStorage(storage) instanceof TextType;
  }

  @Override
  public ColumnStorage<?> apply(
      ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    var mapper = new ObjectMapper();
    var inferredBuilder = Builder.getInferredBuilder(storage.getSize(), problemAggregator);

    return StorageIterators.buildObjectOverStorage(
        TextType.VARIABLE_LENGTH.asTypedStorage(storage),
        true,
        inferredBuilder,
        (builder, index, value) -> {
          try {
            builder.append(parseJson(mapper, value));
          } catch (JsonProcessingException e) {
            problemAggregator.reportInvalidJSONError(value);
            builder.appendNulls(1);
          } catch (IllegalArgumentException e) {
            problemAggregator.reportIllegalArgumentError(
                "Error parsing JSON: " + e.getMessage() + " when parsing " + value, index);
            builder.appendNulls(1);
          }
        });
  }

  private static Object parseJson(ObjectMapper mapper, String json)
      throws JsonProcessingException, IllegalArgumentException {
    var node = mapper.readTree(json);
    return parseJsonNode(node);
  }

  private static Object parseJsonNode(JsonNode node)
      throws JsonProcessingException, IllegalArgumentException {
    return switch (node.getNodeType()) {
      case NULL -> null;
      case BOOLEAN -> node.asBoolean();
      case STRING -> node.asText();
      case NUMBER -> {
        if (node.canConvertToExactIntegral()) {
          yield node.asLong();
        }
        yield node.asDouble();
      }
      case ARRAY -> parseJsonArray(node);
      case OBJECT -> parseJsonObject((ObjectNode) node);
      default ->
          throw new IllegalArgumentException(
              "Unsupported JSON node type: "
                  + node.getNodeType()
                  + " when parsing "
                  + node.asText());
    };
  }

  private static Function<Object[], Value> vectorConstructor;

  private static Value parseJsonArray(JsonNode node) throws JsonProcessingException {
    var array = new Object[node.size()];
    for (int i = 0; i < array.length; i++) {
      array[i] = parseJsonNode(node.get(i));
    }

    if (vectorConstructor == null) {
      var vectorType = EnsoMeta.getType("Standard.Base.Data.Vector", "Vector");
      var method = vectorType.getMember("from_polyglot_array");
      vectorConstructor = arr -> method.execute(vectorType, arr);
    }

    return vectorConstructor.apply(array);
  }

  private static Function<JsonNode, Value> objectConstructor;

  private static Object parseJsonObject(ObjectNode node) throws JsonProcessingException {
    var typeName = getTextValue(node, "type", "");
    return switch (typeName) {
      case "Date" -> parseJsonDate(node);
      case "Time_Of_Day" -> parseJsonTime(node);
      case "Date_Time" -> parseJsonDateTime(node);
      case "Decimal" -> parseJsonDecimal(node);
      case "Integer" -> parseJsonInteger(node);
      case "Float" -> parseJsonFloat(node);
      default -> makeJSObject(node);
    };
  }

  private static Object parseJsonFloat(ObjectNode node) {
    return switch (getTextValue(node, "value", "")) {
      case "Infinity" -> Double.POSITIVE_INFINITY;
      case "-Infinity" -> Double.NEGATIVE_INFINITY;
      case "NaN" -> Double.NaN;
      default -> makeJSObject(node);
    };
  }

  private static Value makeJSObject(ObjectNode node) {
    if (objectConstructor == null) {
      var objectType = EnsoMeta.getType("Standard.Base.Data.Json", "JS_Object");
      var method = objectType.getMember("new");
      objectConstructor = jsonNode -> method.execute(objectType, jsonNode);
    }
    return objectConstructor.apply(node);
  }

  private static int getIntValue(ObjectNode node, String fieldName, int defaultValue) {
    var fieldNode = node.get(fieldName);
    return (fieldNode == null ? defaultValue : fieldNode.asInt(defaultValue));
  }

  private static String getTextValue(ObjectNode node, String fieldName, String defaultValue) {
    var fieldNode = node.get(fieldName);
    return (fieldNode == null ? defaultValue : fieldNode.asText(defaultValue));
  }

  private static Object parseJsonDate(ObjectNode node) {
    var year = getIntValue(node, "year", -1);
    var month = getIntValue(node, "month", -1);
    var day = getIntValue(node, "day", -1);
    if (year == -1 || month == -1 || day == -1) {
      // Invalid Date - so return a JS_Object
      return makeJSObject(node);
    }
    return LocalDate.of(year, month, day);
  }

  private static Object parseJsonTime(ObjectNode node) {
    var hour = getIntValue(node, "hour", -1);
    var minute = getIntValue(node, "minute", -1);
    if (hour == -1 || minute == -1) {
      // Invalid Time_Of_Day - so return a JS_Object
      return makeJSObject(node);
    }
    var second = getIntValue(node, "second", 0);
    var nano = getIntValue(node, "nanosecond", 0);
    return LocalTime.of(hour, minute, second, nano);
  }

  private static Object parseJsonDateTime(ObjectNode node) {
    var date = parseJsonDate(node);
    if (!(date instanceof LocalDate localDate)) {
      return date;
    }

    var time = parseJsonTime(node);
    if (!(time instanceof LocalTime localTime)) {
      return time;
    }

    var localDateTime = localDate.atTime(localTime);

    var timeZone = ZoneId.systemDefault();
    var zoneNode = node.get("zone");
    if (zoneNode != null) {
      var zoneIdNode = zoneNode.get("id");
      if (zoneIdNode == null || !zoneIdNode.isTextual()) {
        // Invalid Time_Zone - so return a JS_Object
        return makeJSObject(node);
      }
      var zoneId = zoneIdNode.asText();
      if (zoneId.equals("UTC") || zoneId.equals("Z")) {
        timeZone = ZoneId.of("UTC");
      } else if (!zoneId.equals(timeZone.getId())) {
        timeZone = ZoneId.of(zoneIdNode.asText());
      }
    }

    return localDateTime.atZone(timeZone);
  }

  private static Object parseJsonDecimal(ObjectNode node) {
    var value = getTextValue(node, "value", null);
    var scale = getIntValue(node, "scale", -1);
    var precision = getIntValue(node, "precision", -1);
    if (value == null || scale == -1 || precision == -1) {
      // Invalid Decimal - so return a JS_Object
      return makeJSObject(node);
    }

    var mathContext = new MathContext(precision, RoundingMode.HALF_UP);
    return new BigDecimal(value, mathContext).setScale(scale, RoundingMode.HALF_UP);
  }

  private static final BigInteger MIN_LONG = BigInteger.valueOf(Long.MIN_VALUE);
  private static final BigInteger MAX_LONG = BigInteger.valueOf(Long.MAX_VALUE);

  private static Object parseJsonInteger(ObjectNode node) {
    var value = getTextValue(node, "value", null);
    if (value == null) {
      // Invalid Integer - so return a JS_Object
      return makeJSObject(node);
    }

    var bigInteger = new BigInteger(value);
    if (bigInteger.compareTo(MIN_LONG) >= 0 && bigInteger.compareTo(MAX_LONG) <= 0) {
      return bigInteger.longValue();
    }
    return bigInteger;
  }
}
