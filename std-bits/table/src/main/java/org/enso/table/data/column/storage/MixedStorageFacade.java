package org.enso.table.data.column.storage;

import org.enso.table.data.column.storage.type.AnyObjectType;

/**
 * Wraps a storage of any type and alters its reported storage to be of type AnyObject.
 *
 * <p>This is used to ensure that we can change a column's type to Mixed without changing its
 * underlying storage unnecessarily.
 */
public class MixedStorageFacade extends AbstractBaseStorage<Object>
    implements ColumnStorageWithInferredStorage {
  private final ColumnStorage<?> underlyingStorage;

  public MixedStorageFacade(ColumnStorage<?> storage) {
    super(AnyObjectType.INSTANCE);
    underlyingStorage = storage;
  }

  @Override
  public long getSize() {
    return underlyingStorage.getSize();
  }

  public ColumnStorage<?> getInferredStorage() {
    return underlyingStorage;
  }

  @Override
  public Object getItemBoxed(long idx) {
    return underlyingStorage.getItemBoxed(idx);
  }
}
