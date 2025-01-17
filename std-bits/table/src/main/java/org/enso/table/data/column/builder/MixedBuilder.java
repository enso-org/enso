package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.MixedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.StorageType;

/** A builder for Mixed columns. It will create a MixedStorage. */
public class MixedBuilder extends ObjectBuilder implements BuilderWithRetyping {
  /** Creates a new builder with the given size. Copies the data from the given source Builder. */
  static MixedBuilder fromBuilder(Builder source, int capacity) {
    var sourceCurrentSize = source.getCurrentSize();

    var dataSize = Math.max(capacity, sourceCurrentSize);
    var builder = new MixedBuilder(dataSize);

    source.copyDataTo(builder.data);
    builder.currentSize = sourceCurrentSize;

    return builder;
  }

  MixedBuilder(int size) {
    super(size);
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
