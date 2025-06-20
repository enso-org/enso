package org.enso.table.data.column.operation.masks;

import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.table.Column;

/**
 * Provides operations for slicing columns, allowing the creation of new columns that are sub-ranges
 * of existing ones. A slice is a contiguous part of a column, defined by a starting index and a
 * length.
 */
public final class SliceOperation {
  /**
   * Creates a new Column that contains a slice of the input.
   *
   * @param column the original column
   * @param start the starting index of the slice (inclusive)
   * @param length the total length of the slice
   * @return a new column containing the specified slice
   */
  public static Column slice(Column column, long start, long length) {
    long currentSize = column.getSize();
    long newSize = Math.max(0, Math.min(currentSize - start, length));
    if (start == 0 && newSize == currentSize) {
      // No need to slice, return the original storage
      return column;
    }

    var storage = column.getStorage();
    var newStorage = getSlicedStorage(storage, new IndexMapper.SingleSlice(start, newSize));
    return new Column(column.getName(), newStorage);
  }

  /**
   * Creates a new Column that contains a slice of the input.
   *
   * @param column the original column
   * @param mask the set of indices to include in the slice
   * @return a new column containing the specified slice
   */
  public static Column slice(Column column, long[] mask) {
    var storage = column.getStorage();
    var newStorage = getSlicedStorage(storage, new IndexMapper.ArrayMapping(mask));
    return new Column(column.getName(), newStorage);
  }

  private static ColumnStorage<?> getSlicedStorage(
      ColumnStorage<?> storage, IndexMapper indexMapper) {
    return switch (storage) {
      case SliceStorageLong sliceStorageLong -> new SliceStorageLong(
          sliceStorageLong.parent(), sliceStorageLong.indexMapper().merge(indexMapper));
      case ColumnLongStorage longStorage -> new SliceStorageLong(longStorage, indexMapper);
      case SliceStorageDouble sliceStorageDouble -> new SliceStorageDouble(
          sliceStorageDouble.parent(), sliceStorageDouble.indexMapper().merge(indexMapper));
      case ColumnDoubleStorage doubleStorage -> new SliceStorageDouble(doubleStorage, indexMapper);
      case SliceStorageBoolean sliceStorageBoolean -> new SliceStorageBoolean(
          sliceStorageBoolean.parent(), sliceStorageBoolean.indexMapper().merge(indexMapper));
      case ColumnBooleanStorage booleanStorage -> new SliceStorageBoolean(
          booleanStorage, indexMapper);
      case SliceStorageInferred<?> sliceStorageInferred -> new SliceStorageInferred<>(
          sliceStorageInferred.parent(), sliceStorageInferred.indexMapper().merge(indexMapper));
      case ColumnStorageWithInferredStorage inferredStorage -> new SliceStorageInferred<>(
          storage, indexMapper);
      case SliceStorage<?> sliceStorage -> new SliceStorage<>(
          sliceStorage.parent(), sliceStorage.indexMapper().merge(indexMapper));
      default -> new SliceStorage<>(storage, indexMapper);
    };
  }
}
