package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.builder.BoolBuilder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BooleanType;

public class NotOperation extends AbstractUnaryBooleanOperation {
  public static final String NAME = "not";

  public static final UnaryOperation INSTANCE = new NotOperation();

  private NotOperation() {
    super(NAME, false);
  }

  @Override
  public boolean canApply(ColumnStorage storage) {
    return storage.getType() instanceof BooleanType;
  }

  @Override
  public ColumnStorage apply(
      ColumnStorage storage, MapOperationProblemAggregator problemAggregator) {
    if (storage instanceof BoolStorage boolStorage) {
      return boolStorage.makeNegated();
    }

    var builder = createBuilder(storage, problemAggregator);
    if (storage instanceof ColumnBooleanStorage booleanStorage) {
      UnaryOperation.applyOverBooleanStorage(
          booleanStorage, true, builder, (isNothing, value) -> builder.appendBoolean(!value));
    } else {
      UnaryOperation.applyOverObjectStorage(
          storage,
          true,
          builder,
          (value) -> {
            if (value instanceof Boolean b) {
              builder.appendBoolean(!b);
            } else {
              throw new IllegalArgumentException(
                  "Unsupported type: " + value.getClass() + " (expected boolean type).");
            }
          });
    }

    return builder.seal();
  }

  @Override
  protected void applyObjectRow(
      Object value, BoolBuilder builder, MapOperationProblemAggregator problemAggregator) {
    throw new UnsupportedOperationException();
  }
}
