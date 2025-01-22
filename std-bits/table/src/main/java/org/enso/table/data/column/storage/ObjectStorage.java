package org.enso.table.data.column.storage;

import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;

/** A column storing arbitrary Java objects. */
public sealed class ObjectStorage extends SpecializedStorage<Object> permits MixedStorage {
  /** An empty object storage. */
  public static ObjectStorage EMPTY = new ObjectStorage(new Object[0]);

  /**
   * @param data the underlying data
   */
  public ObjectStorage(Object[] data) {
    super(AnyObjectType.INSTANCE, data, new MapOperationStorage<>());
  }

  @Override
  protected SpecializedStorage<Object> newInstance(Object[] data) {
    return new ObjectStorage(data);
  }

  @Override
  protected Object[] newUnderlyingArray(int size) {
    return new Object[size];
  }
}
