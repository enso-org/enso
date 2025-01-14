package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.MixedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.StorageType;

/** A builder for Mixed columns. It will create a MixedStorage. */
public class MixedBuilder extends ObjectBuilder implements BuilderWithRetyping {
  public MixedBuilder(int size) {
    super(size);
  }

  public MixedBuilder(Object[] data) {
    super(data);
  }

  @Override
  public Storage<Object> seal() {
    resize(currentSize);
    return new MixedStorage(data, currentSize);
  }

  @Override
  public boolean accepts(Object o) {
    return true;
  }

  @Override
  public boolean canRetypeTo(StorageType type) {
    return false;
  }

  @Override
  public Builder retypeTo(StorageType type) {
    throw new UnsupportedOperationException("MixedBuilder cannot be re-typed.");
  }
}
