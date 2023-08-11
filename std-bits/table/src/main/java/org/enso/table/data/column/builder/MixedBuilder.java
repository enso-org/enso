package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.MixedStorage;
import org.enso.table.data.column.storage.Storage;

/** A builder for Mixed columns. It will create a MixedStorage. */
public class MixedBuilder extends ObjectBuilder {
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
}
