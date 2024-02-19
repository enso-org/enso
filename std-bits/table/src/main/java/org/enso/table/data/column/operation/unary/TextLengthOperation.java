package org.enso.table.data.column.operation.unary;

import org.enso.base.Text_Utils;
import org.enso.table.data.column.builder.LongBuilder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.TextType;

public class TextLengthOperation extends AbstractUnaryLongOperation {
  public static final String NAME = "text_length";
  public static final UnaryOperation INSTANCE = new TextLengthOperation();

  private TextLengthOperation() {
    super(NAME, true, IntegerType.INT_64);
  }

  @Override
  public boolean canApply(ColumnStorage storage) {
    return storage.getType() instanceof TextType;
  }

  @Override
  protected void applyObjectRow(
      Object value, LongBuilder builder, MapOperationProblemAggregator problemAggregator) {
    if (value instanceof String s) {
      var longValue = Text_Utils.grapheme_length(s);
      builder.appendLong(longValue);
    } else {
      throw new IllegalArgumentException(
          "Unsupported type: " + value.getClass() + " (expected text type).");
    }
  }
}
