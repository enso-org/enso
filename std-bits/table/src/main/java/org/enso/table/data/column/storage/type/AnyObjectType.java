package org.enso.table.data.column.storage.type;

import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.problems.ProblemAggregator;

public final class AnyObjectType implements StorageType<Object> {
  public static final AnyObjectType INSTANCE = new AnyObjectType();

  private AnyObjectType() {}

  @Override
  public char typeChar() {
    return 'A';
  }

  @Override
  public boolean isOfType(StorageType<?> other) {
    return other instanceof AnyObjectType;
  }

  @Override
  public Object valueAsType(Object value) {
    return Polyglot_Utils.convertPolyglotValue(value);
  }

  @Override
  public BuilderForType<Object> makeBuilder(
      long initialCapacity, ProblemAggregator problemAggregator) {
    return Builder.getForAnyObject(initialCapacity);
  }

  @Override
  public ColumnStorage<Object> asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof AnyObjectType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnStorage<Object>) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of AnyObjectType");
  }
}
