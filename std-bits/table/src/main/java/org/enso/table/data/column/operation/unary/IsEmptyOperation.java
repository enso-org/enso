package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.builder.BoolBuilder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
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
    var type = storage.getType();
    // We also allow this operation on Mixed type to facilitate `internal_is_empty` helper.
    return type instanceof TextType || type instanceof AnyObjectType;
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
        builder.appendBoolean(false);
      }
    }
  }
}
