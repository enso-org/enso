package org.enso.table.data.column.operation.unary;

import java.util.BitSet;
import org.enso.table.data.column.builder.BoolBuilder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithNothingMap;

public class IsNothingOperation extends AbstractUnaryBooleanOperation {
  public static final String NAME = "is_nothing";

  public static final UnaryOperation INSTANCE = new IsNothingOperation();

  private IsNothingOperation() {
    super(NAME, false);
  }

  @Override
  public boolean canApply(ColumnStorage storage) {
    return true;
  }

  @Override
  public ColumnStorage apply(
      ColumnStorage storage, MapOperationProblemAggregator problemAggregator) {
    if (storage instanceof ColumnStorageWithNothingMap withNothingMap) {
      return new BoolStorage(
          withNothingMap.getIsNothingMap(), new BitSet(), (int) storage.getSize(), false);
    }

    var builder = createBuilder(storage, problemAggregator);
    for (long i = 0; i < storage.getSize(); i++) {
      builder.appendBoolean(storage.isNothing(i));
    }
    return builder.seal();
  }

  @Override
  protected void applyObjectRow(
      Object value, BoolBuilder builder, MapOperationProblemAggregator problemAggregator) {
    throw new UnsupportedOperationException();
  }
}
