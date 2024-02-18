package org.enso.table.data.column.operation.unary;

import org.enso.base.Text_Utils;
import org.enso.table.data.column.builder.LongBuilder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.problems.ProblemAggregator;

import java.time.temporal.Temporal;

public class TextLengthOperation extends AbstractUnaryLongOperation {
  public static final UnaryOperation INSTANCE = new TextLengthOperation();

  private TextLengthOperation() {
    super(UnaryOperation.TEXT_LENGTH, true, IntegerType.INT_64);
  }

  @Override
  public boolean canApply(ColumnStorage storage) {
    return storage.getType() instanceof TextType;
  }

  @Override
  protected void applyObjectRow(Object value, LongBuilder builder, ProblemAggregator problemAggregator) {
    if (value instanceof String s) {
      var longValue = Text_Utils.grapheme_length(s);
      builder.appendLong(longValue);
    } else {
      throw new IllegalArgumentException(STR."Unsupported type: \{value.getClass()}");
    }
  }
}
