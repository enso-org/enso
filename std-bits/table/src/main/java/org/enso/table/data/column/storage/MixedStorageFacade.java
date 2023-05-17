package org.enso.table.data.column.storage;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.operation.CastProblemBuilder;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;

import java.util.BitSet;
import java.util.List;

/**
 * Wraps a storage of any type and alters its reported storage to be of type AnyObject.
 *
 * <p>This is used to ensure that we can change a column's type to Mixed without changing its
 * underlying storage unnecessarily.
 */
public class MixedStorageFacade extends Storage<Object> {
  private final Storage<?> underlyingStorage;

  public MixedStorageFacade(Storage<?> storage) {
    underlyingStorage = storage;
  }

  @Override
  public int size() {
    return underlyingStorage.size();
  }

  @Override
  public int countMissing() {
    return underlyingStorage.countMissing();
  }

  @Override
  public StorageType getType() {
    return AnyObjectType.INSTANCE;
  }

  @Override
  public boolean isNa(long idx) {
    return underlyingStorage.isNa(idx);
  }

  @Override
  public Object getItemBoxed(int idx) {
    return underlyingStorage.getItemBoxed(idx);
  }

  @Override
  public boolean isOpVectorized(String name) {
    return underlyingStorage.isOpVectorized(name);
  }

  @Override
  protected Storage<?> runVectorizedMap(
      String name, Object argument, MapOperationProblemBuilder problemBuilder) {
    return underlyingStorage.runVectorizedMap(name, argument, problemBuilder);
  }

  @Override
  protected Storage<?> runVectorizedZip(
      String name, Storage<?> argument, MapOperationProblemBuilder problemBuilder) {
    return underlyingStorage.runVectorizedZip(name, argument, problemBuilder);
  }

  @Override
  public Storage<Object> mask(BitSet mask, int cardinality) {
    Storage<?> newStorage = underlyingStorage.mask(mask, cardinality);
    return new MixedStorageFacade(newStorage);
  }

  @Override
  public Storage<Object> applyMask(OrderMask mask) {
    Storage<?> newStorage = underlyingStorage.applyMask(mask);
    return new MixedStorageFacade(newStorage);
  }

  @Override
  public Storage<Object> countMask(int[] counts, int total) {
    Storage<?> newStorage = underlyingStorage.countMask(counts, total);
    return new MixedStorageFacade(newStorage);
  }

  @Override
  public Storage<Object> slice(int offset, int limit) {
    Storage<?> newStorage = underlyingStorage.slice(offset, limit);
    return new MixedStorageFacade(newStorage);
  }

  @Override
  public Builder createDefaultBuilderOfSameType(int capacity) {
    throw new UnsupportedOperationException("TODO");
  }

  @Override
  public Storage<Object> slice(List<SliceRange> ranges) {
    Storage<?> newStorage = underlyingStorage.slice(ranges);
    return new MixedStorageFacade(newStorage);
  }

  @Override
  public Storage<?> cast(StorageType targetType, CastProblemBuilder castProblemBuilder) {
    return null;
  }
}
