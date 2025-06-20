package org.enso.table.data.column.storage;

import java.util.BitSet;
import java.util.List;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;

/**
 * Wraps a storage of any type and alters its reported storage to be of type AnyObject.
 *
 * <p>This is used to ensure that we can change a column's type to Mixed without changing its
 * underlying storage unnecessarily.
 */
public class MixedStorageFacade extends Storage<Object>
    implements ColumnStorageWithInferredStorage {
  private final Storage<?> underlyingStorage;

  public MixedStorageFacade(ColumnStorage<?> storage) {
    underlyingStorage = (Storage<?>) storage;
  }

  @Override
  public long getSize() {
    return underlyingStorage.getSize();
  }

  @Override
  public StorageType<Object> getType() {
    return AnyObjectType.INSTANCE;
  }

  public ColumnStorage<?> getInferredStorage() {
    return underlyingStorage;
  }

  @Override
  public boolean isNothing(long idx) {
    return underlyingStorage.isNothing(idx);
  }

  @Override
  public Object getItemBoxed(long idx) {
    return underlyingStorage.getItemBoxed(idx);
  }

  @Override
  public ColumnStorage<Object> applyFilter(BitSet filterMask, int newLength) {
    var newStorage = underlyingStorage.applyFilter(filterMask, newLength);
    return new MixedStorageFacade(newStorage);
  }

  @Override
  public ColumnStorage<Object> applyMask(OrderMask mask) {
    var newStorage = underlyingStorage.applyMask(mask);
    return new MixedStorageFacade(newStorage);
  }

  @Override
  public ColumnStorage<Object> slice(int offset, int limit) {
    var newStorage = underlyingStorage.slice(offset, limit);
    return new MixedStorageFacade(newStorage);
  }

  @Override
  public ColumnStorage<Object> slice(List<SliceRange> ranges) {
    var newStorage = underlyingStorage.slice(ranges);
    return new MixedStorageFacade(newStorage);
  }
}
