package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.builder.BoolBuilder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.problems.ProblemAggregator;

/* An operation that checks if a column's row values are empty.
* Only supported on Text values. */
public class IsEmptyOperation extends AbstractUnaryBooleanOperation {
  public static final UnaryOperation INSTANCE = new IsEmptyOperation();

  private IsEmptyOperation() {
    super(UnaryOperation.IS_EMPTY, false);
  }

  @Override
  public boolean canApply(ColumnStorage storage) {
    return storage.getType() instanceof TextType;
  }

  @Override
  protected void applyObjectRow(Object value, BoolBuilder builder, ProblemAggregator problemAggregator) {
    if (value == null) {
      builder.appendBoolean(true);
    } else {
      if (value instanceof String s) {
        builder.appendBoolean(s.isEmpty());
      } else {
        throw new IllegalArgumentException("Unsupported type: "+value.getClass() + " (expected text type).");
      }
    }
  }
}
