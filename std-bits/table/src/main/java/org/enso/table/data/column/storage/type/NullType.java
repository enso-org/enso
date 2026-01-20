package org.enso.table.data.column.storage.type;

import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.problems.ProblemAggregator;

public final class NullType implements StorageType<Void> {
  public static final NullType INSTANCE = new NullType();

  private NullType() {}

  @Override
  public char typeChar() {
    return 'N';
  }

  @Override
  public boolean isNumeric() {
    return true;
  }

  @Override
  public boolean hasDate() {
    return true;
  }

  @Override
  public boolean hasTime() {
    return true;
  }

  @Override
  public boolean isOfType(StorageType<?> other) {
    return other instanceof NullType;
  }

  @Override
  public Void valueAsType(Object value) {
    throw new UnsupportedOperationException("Cannot cast to a NullType value.");
  }

  @Override
  public BuilderForType<Void> makeBuilder(
      long initialCapacity, ProblemAggregator problemAggregator) {
    throw new UnsupportedOperationException("Cannot make a builder for NullType");
  }

  @Override
  public ColumnStorage<Void> asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof NullType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnStorage<Void>) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of NullType");
  }
}
