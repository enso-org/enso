package org.enso.table.data.column.operation.unary;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
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
            problemAggregator.reportIllegalArgumentError(
                "Failed to parse JSON: " + e.getMessage(), index);
            builder.appendNulls(1);
          } catch (IllegalArgumentException e) {
            problemAggregator.reportIllegalArgumentError(
                "Unsupported JSON node type: " + e.getMessage() + " when parsing " + value, index);
            builder.appendNulls(1);
          }
        });
  }

  private static Object parseJson(ObjectMapper mapper, String json) throws JsonProcessingException {
    // ToDo: Object
    var node = mapper.readTree(json);
    return parseJsonNode(node);
  }

  private static Object parseJsonNode(JsonNode node) throws JsonProcessingException {
    return switch (node.getNodeType()) {
      case NULL -> null;
      case BOOLEAN -> node.asBoolean();
      case STRING -> node.asText();
      case NUMBER -> node.isIntegralNumber() ? node.asLong() : node.asDouble();
      case ARRAY -> parseJsonArray(node);
      default -> throw new IllegalArgumentException(
          "Unsupported JSON node type: " + node.getNodeType() + " when parsing " + node.asText());
    };
  }

  private static Function<Object[], Value> vectorConstructor;

  private static Value parseJsonArray(JsonNode node)
      throws JsonProcessingException {
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
}
