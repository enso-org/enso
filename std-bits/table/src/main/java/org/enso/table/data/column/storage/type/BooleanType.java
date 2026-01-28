package org.enso.table.data.column.storage.type;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForBoolean;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Value;

public final class BooleanType implements StorageType<Boolean> {
  public static final BooleanType INSTANCE = new BooleanType();

  private BooleanType() {}

  @Override
  public char typeChar() {
    return 'B';
  }

  @Override
  public boolean isOfType(StorageType<?> other) {
    return other instanceof BooleanType;
  }

  @Override
  public Boolean valueAsType(Object value) {
    if (value instanceof Boolean boolValue) {
      return boolValue;
    }

    if (value instanceof Value polyglotValue && polyglotValue.isBoolean()) {
      return polyglotValue.asBoolean();
    }

    return null;
  }

  @Override
  public BuilderForBoolean makeBuilder(long initialCapacity, ProblemAggregator problemAggregator) {
    return Builder.getForBoolean(initialCapacity);
  }

  @Override
  public ColumnBooleanStorage asTypedStorage(ColumnStorage<?> storage) {
    if (StorageType.ofStorage(storage) instanceof BooleanType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnBooleanStorage) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of BooleanType");
  }
}
