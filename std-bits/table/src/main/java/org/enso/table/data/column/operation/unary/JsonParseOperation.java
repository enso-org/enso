package org.enso.table.data.column.operation.unary;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

/**
 * Deserializes JSON to Objects in the Table.
 */
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
  public ColumnStorage<?> apply(ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    var mapper = new ObjectMapper();
    var inferredBuilder = Builder.getInferredBuilder(storage.getSize(), problemAggregator);

    return StorageIterators.buildObjectOverStorage(
        TextType.VARIABLE_LENGTH.asTypedStorage(storage),
        true,
        inferredBuilder,
        ( builder, index, value) -> {
          try {
            builder.append(parseJson(mapper, value));
          } catch (JsonProcessingException e) {
            problemAggregator.reportIllegalArgumentError("Failed to parse JSON: " + e.getMessage(), index);
            builder.appendNulls(1);
          }
        });
  }

  private static Object parseJson(ObjectMapper mapper, String json) throws JsonProcessingException {
    // ToDo: Array
    // ToDo: Object
    var node = mapper.readTree(json);
    return switch (node.getNodeType()) {
      case NULL -> null;
      case BOOLEAN -> node.asBoolean();
      case STRING -> node.asText();
      case NUMBER -> node.isIntegralNumber() ?  node.asLong() : node.asDouble();
      default -> throw new IllegalArgumentException("Unsupported JSON node type: " + node.getNodeType() + " when parsing " + json);
    };
  }
}
