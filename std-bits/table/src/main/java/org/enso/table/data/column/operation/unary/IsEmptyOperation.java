package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.builder.BoolBuilder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.TextType;

/* An operation that checks if a column's row values are empty.
 * Only supported on Text values. */
public class IsEmptyOperation extends AbstractUnaryBooleanOperation {
  public static String NAME = "is_empty";

  public static final UnaryOperation INSTANCE = new IsEmptyOperation();

  private IsEmptyOperation() {
    super(NAME, false);
  }

  @Override
  public boolean canApply(ColumnStorage storage) {
    return storage.getType() instanceof TextType;
  }

  @Override
  protected void applyObjectRow(
      Object value, BoolBuilder builder, MapOperationProblemAggregator problemAggregator) {
    if (value == null) {
      builder.appendBoolean(true);
    } else {
      if (value instanceof String s) {
        builder.appendBoolean(s.isEmpty());
      } else {
        throw new IllegalArgumentException(
            "Unsupported type: " + value.getClass() + " (expected text type).");
      }
    }
  }
}
