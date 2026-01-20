package org.enso.table.data.column.operation.masks;

import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.table.Column;

/**
 * Provides operations for masking and slicing columns, allowing the creation of new columns that
 * are sub-ranges or reordering of existing ones. An IndexMapper is used to define how the indices
 * of the original column map to the new column.
 */
public final class MaskOperation {
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
   * Creates a new Column that contains a masked version of the input.
   *
   * @param column the original column
   * @param mask the set of indices to include in the mask
   * @return a new column containing the specified mask
   */
  public static Column mask(Column column, long[] mask) {
    var storage = column.getStorage();
    var newStorage = getSlicedStorage(storage, new IndexMapper.ArrayMapping(mask));
    return new Column(column.getName(), newStorage);
  }

  /**
   * Creates a new ColumnStorage that contains a mask of the input.
   *
   * @param storage the original storage
   * @param indexMapper the index mapper defining how to map indices from the original storage
   * @return a new ColumnStorage containing the masked or sliced version of the input storage
   */
  public static ColumnStorage<?> getSlicedStorage(
      ColumnStorage<?> storage, IndexMapper indexMapper) {
    return switch (storage) {
      case MaskedStorageLong sliceStorageLong ->
          new MaskedStorageLong(
              sliceStorageLong.parent(), sliceStorageLong.indexMapper().merge(indexMapper));
      case ColumnLongStorage longStorage -> new MaskedStorageLong(longStorage, indexMapper);
      case MaskedStorageDouble sliceStorageDouble ->
          new MaskedStorageDouble(
              sliceStorageDouble.parent(), sliceStorageDouble.indexMapper().merge(indexMapper));
      case ColumnDoubleStorage doubleStorage -> new MaskedStorageDouble(doubleStorage, indexMapper);
      case MaskedStorageBoolean sliceStorageBoolean ->
          new MaskedStorageBoolean(
              sliceStorageBoolean.parent(), sliceStorageBoolean.indexMapper().merge(indexMapper));
      case ColumnBooleanStorage booleanStorage ->
          new MaskedStorageBoolean(booleanStorage, indexMapper);
      case MaskedStorageInferred<?> sliceStorageInferred ->
          new MaskedStorageInferred<>(
              sliceStorageInferred.parent(), sliceStorageInferred.indexMapper().merge(indexMapper));
      case ColumnStorageWithInferredStorage inferredStorage ->
          new MaskedStorageInferred<>(storage, indexMapper);
      case MaskedStorage<?> sliceStorage ->
          new MaskedStorage<>(sliceStorage.parent(), sliceStorage.indexMapper().merge(indexMapper));
      default -> new MaskedStorage<>(storage, indexMapper);
    };
  }
}
