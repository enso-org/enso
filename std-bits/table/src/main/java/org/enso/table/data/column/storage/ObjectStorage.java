package org.enso.table.data.column.storage;

import org.enso.table.data.column.storage.type.AnyObjectType;

/** A column storing arbitrary Java objects. */
public sealed class ObjectStorage extends SpecializedStorage<Object> permits MixedStorage {
  /**
   * @param data the underlying data
   */
  public ObjectStorage(Object[] data) {
    super(AnyObjectType.INSTANCE, data);
  }
}
