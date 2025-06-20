package org.enso.table.data.column.operation.masks;

import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.table.Column;

/**
 * Provides operations for slicing columns, allowing the creation of new columns that are
 * sub-ranges of existing ones.
 * A slice is a contiguous part of a column, defined by a starting index and a length.
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
    var newStorage = getSlicedStorage(start, length, storage, newSize);
    return new Column(column.getName(), newStorage);
  }

  private static ColumnStorage<?> getSlicedStorage(
      long start, long length, ColumnStorage<?> storage, long newSize) {
    if (storage instanceof ColumnLongStorage longStorage) {
      // Handle slicing for long storage
      return getSlicedLongStorage(start, length, longStorage, newSize);
    }

    if (storage instanceof ColumnDoubleStorage doubleStorage) {
      // Handle slicing for double storage
      return getSlicedDoubleStorage(start, length, doubleStorage, newSize);
    }

    if (storage instanceof ColumnBooleanStorage booleanStorage) {
      // Handle slicing for boolean storage
      return getSlicedBooleanStorage(start, length, booleanStorage, newSize);
    }

    if (storage instanceof ColumnStorageWithInferredStorage) {
      if (storage instanceof SliceStorage<?> sliceStorage) {
        // Avoid nesting sliced storages
        var oldParent = sliceStorage.parent();
        long newStart = sliceStorage.start + start;
        long newEnd = sliceStorage.start + length;
        return new SliceStorageInferred<>(oldParent, newStart, newEnd);
      }

      // Create a new SliceStorage wrapping the original storage
      return new SliceStorageInferred<>(storage, start, start + newSize);

    }

    if (storage instanceof SliceStorage<?> sliceStorage) {
      // Avoid nesting sliced storages
      var oldParent = sliceStorage.parent();
      long newStart = sliceStorage.start + start;
      long newEnd = sliceStorage.start + length;
      return new SliceStorage<>(oldParent, newStart, newEnd);
    }

    // Create a new SliceStorage wrapping the original storage
    return new SliceStorage<>(storage, start, start + newSize);
  }

  private static ColumnLongStorage getSlicedLongStorage(
      long start, long length, ColumnLongStorage storage, long newSize) {
    if (storage instanceof SliceStorageLong sliceLongStorage) {
      // Avoid nesting sliced storages
      long newStart = sliceLongStorage.start + start;
      long newEnd = sliceLongStorage.start + length;
      return new SliceStorageLong(sliceLongStorage.parent(), newStart, newEnd);
    }
    return new SliceStorageLong(storage, start, start + newSize);
  }

  private static ColumnDoubleStorage getSlicedDoubleStorage(
      long start, long length, ColumnDoubleStorage storage, long newSize) {
    if (storage instanceof SliceStorageDouble sliceDoubleStorage) {
      // Avoid nesting sliced storages
      long newStart = sliceDoubleStorage.start + start;
      long newEnd = sliceDoubleStorage.start + length;
      return new SliceStorageDouble(sliceDoubleStorage.parent(), newStart, newEnd);
    }
    return new SliceStorageDouble(storage, start, start + newSize);
  }

  private static ColumnBooleanStorage getSlicedBooleanStorage(
      long start, long length, ColumnBooleanStorage storage, long newSize) {
    if (storage instanceof SliceStorageBoolean sliceBooleanStorage) {
      // Avoid nesting sliced storages
      long newStart = sliceBooleanStorage.start + start;
      long newEnd = sliceBooleanStorage.start + length;
      return new SliceStorageBoolean(sliceBooleanStorage.parent(), newStart, newEnd);
    }
    return new SliceStorageBoolean(storage, start, start + newSize);
  }
}
