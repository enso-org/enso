package org.enso.table_test_helpers;

import java.util.BitSet;
import java.util.List;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;

/**
 * A helper class used in the Upload_Spec test to purposefully interrupt a table upload in the
 * middle of it by throwing an exception. It is used to test the transactionality of the upload.
 */
public class ExplodingStorage extends Storage<Long> {
  private final long[] array;
  private final long explodingIndex;

  public ExplodingStorage(long[] array, long explodingIndex) {
    this.array = array;
    this.explodingIndex = explodingIndex;
  }

  private void checkIndex(long idx) {
    if (idx == explodingIndex) {
      throw new ExplodingStoragePayload();
    }
  }

  @Override
  public long getSize() {
    return array.length;
  }

  @Override
  public StorageType<Long> getType() {
    return IntegerType.INT_64;
  }

  @Override
  public boolean isNothing(long idx) {
    checkIndex(idx);
    return false;
  }

  @Override
  public Long getItemBoxed(long idx) {
    checkIndex(idx);
    return array[Math.toIntExact(idx)];
  }

  @Override
  public ColumnStorage<?> fillMissingFromPrevious(BoolStorage missingIndicator) {
    return null;
  }

  @Override
  public ColumnStorage<Long> applyFilter(BitSet filterMask, int newLength) {
    return null;
  }

  @Override
  public ColumnStorage<Long> applyMask(OrderMask mask) {
    return null;
  }

  @Override
  public ColumnStorage<Long> slice(int offset, int limit) {
    return null;
  }

  @Override
  public ColumnStorage<Long> slice(List<SliceRange> ranges) {
    return null;
  }
}
