package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.builder.BoolBuilder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithNothingMap;
import org.enso.table.problems.ProblemAggregator;

import java.util.BitSet;

public class IsNothingOperation implements UnaryOperation {
  public static final UnaryOperation INSTANCE = new IsNothingOperation();

  private IsNothingOperation() {
  }

  @Override
  public String getName() {
    return UnaryOperation.IS_NOTHING;
  }

  @Override
  public boolean canApply(ColumnStorage storage) {
    return true;
  }

  @Override
  public ColumnStorage apply(ColumnStorage storage, ProblemAggregator problemAggregator) {
    if (storage.getSize() > Integer.MAX_VALUE) {
      throw new IllegalArgumentException(STR."Cannot currently operate on columns larger than \{Integer.MAX_VALUE}.");
    }

    if (storage instanceof ColumnStorageWithNothingMap withNothingMap) {
      return new BoolStorage(withNothingMap.getIsNothingMap(), new BitSet(), (int)storage.getSize(), false);
    }

    var builder = new BoolBuilder((int)storage.getSize());
    for (long i = 0; i < storage.getSize(); i++) {
      builder.appendBoolean(storage.isNothing(i));
    }
    return builder.seal();
  }
}
