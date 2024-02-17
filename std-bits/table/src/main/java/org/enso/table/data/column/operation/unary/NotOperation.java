package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.builder.BoolBuilder;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.problems.ProblemAggregator;

public class NotOperation extends AbstractUnaryBooleanOperation {
  public static final UnaryOperation INSTANCE = new NotOperation();

  private NotOperation() {
    super(UnaryOperation.NOT, false);
  }

  @Override
  public boolean canApply(ColumnStorage storage) {
    return storage.getType() instanceof BooleanType;
  }

  @Override
  public ColumnStorage apply(ColumnStorage storage, ProblemAggregator problemAggregator) {
    if (storage instanceof BoolStorage boolStorage) {
      return new BoolStorage(boolStorage.getValues(), boolStorage.getIsNothingMap(), boolStorage.size(), !boolStorage.isNegated());
    }

    var builder = createBuilder(storage, problemAggregator);
    if (storage instanceof ColumnBooleanStorage booleanStorage) {
      UnaryOperation.applyOverBooleanStorage(booleanStorage, true, builder, (isNothing, value) -> builder.appendBoolean(!value));
    } else {
      UnaryOperation.applyOverObjectStorage(storage, true, builder, (value) -> {
        if (value instanceof Boolean b) {
          builder.appendBoolean(!b);
        } else {
          throw new IllegalArgumentException(STR."Unsupported type: \{value.getClass()}");
        }
      });
    }

    return builder.seal();
  }

  @Override
  protected void applyObjectRow(Object value, Builder builder, ProblemAggregator problemAggregator) {
    throw new UnsupportedOperationException();
  }
}
