package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.TextType;

/* An operation that checks if a column's row values are empty.
 * Only supported on Text values. */
public class IsEmptyOperation implements UnaryOperation {
  public static String NAME = "is_empty";

  public static final UnaryOperation INSTANCE = new IsEmptyOperation();

  private IsEmptyOperation() {}

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  public boolean canApply(ColumnStorage<?> storage) {
    var type = storage.getType();
    // We also allow this operation on Mixed type to facilitate `internal_is_empty` helper.
    return type instanceof TextType || type instanceof AnyObjectType;
  }

  @Override
  public ColumnStorage<?> apply(
      ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    return StorageIterators.buildOverStorage(
        storage,
        false,
        Builder.getForBoolean(storage.getSize()),
        (builder, index, value) ->
            builder.appendBoolean(value == null || value instanceof String s && s.isEmpty()));
  }
}
