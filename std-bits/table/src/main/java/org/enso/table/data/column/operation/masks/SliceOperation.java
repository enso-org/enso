package org.enso.table.data.column.operation.masks;

import java.util.Iterator;
import java.util.NoSuchElementException;
import org.enso.table.data.column.storage.*;
import org.enso.table.data.column.storage.iterators.*;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;

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

    if (storage instanceof SliceStorage<?> sliceStorage) {
      // Avoid nesting sliced storages
      var oldParent = sliceStorage.parent;
      long newStart = sliceStorage.start + start;
      long newEnd = sliceStorage.start + length;
      return new SliceStorage<>(oldParent, newStart, newEnd);
    }

    // Create a new SliceStorage wrapping the original storage
    return new SliceStorage<>(storage, start, start + newSize);
  }

  private static ColumnLongStorage getSlicedLongStorage(
      long start, long length, ColumnLongStorage storage, long newSize) {
    if (storage instanceof SliceLongStorage sliceLongStorage) {
      // Avoid nesting sliced storages
      long newStart = sliceLongStorage.start + start;
      long newEnd = sliceLongStorage.start + length;
      return new SliceLongStorage(sliceLongStorage.parent, newStart, newEnd);
    }
    return new SliceLongStorage(storage, start, start + newSize);
  }

  private static ColumnDoubleStorage getSlicedDoubleStorage(
      long start, long length, ColumnDoubleStorage storage, long newSize) {
    if (storage instanceof SliceDoubleStorage sliceDoubleStorage) {
      // Avoid nesting sliced storages
      long newStart = sliceDoubleStorage.start + start;
      long newEnd = sliceDoubleStorage.start + length;
      return new SliceDoubleStorage(sliceDoubleStorage.parent, newStart, newEnd);
    }
    return new SliceDoubleStorage(storage, start, start + newSize);
  }

  private static class SliceStorage<T> implements ColumnStorage<T> {
    private final ColumnStorage<T> parent;
    protected final long start;
    protected final long end;

    public SliceStorage(ColumnStorage<T> parent, long start, long end) {
      this.parent = parent;
      this.start = start;
      this.end = end;
    }

    @Override
    public long uniqueKey() {
      return parent.uniqueKey();
    }

    @Override
    public long getSize() {
      return 0;
    }

    @Override
    public StorageType<T> getType() {
      return parent.getType();
    }

    @Override
    public boolean isNothing(long index) {
      if (index < 0 || index >= end - start) {
        throw new IndexOutOfBoundsException(index);
      }
      return parent.isNothing(index - start);
    }

    @Override
    public T getItemBoxed(long index) {
      if (index < 0 || index >= end - start) {
        throw new IndexOutOfBoundsException(index);
      }
      return parent.getItemBoxed(index - start);
    }

    @Override
    public Iterator<T> iterator() {
      return new Iterator<>() {
        private long index = -1;

        @Override
        public boolean hasNext() {
          return index + 1 < end;
        }

        @Override
        public T next() {
          if (!hasNext()) {
            throw new NoSuchElementException();
          }
          return getItemBoxed(++index);
        }
      };
    }
  }

  private static class SliceLongStorage extends SliceStorage<Long> implements ColumnLongStorage {
    private final ColumnLongStorage parent;

    public SliceLongStorage(ColumnLongStorage parent, long start, long end) {
      super(parent, start, end);
      this.parent = parent;
    }

    @Override
    public IntegerType getType() {
      return parent.getType();
    }

    @Override
    public long getItemAsLong(long index) throws ValueIsNothingException {
      if (index < 0 || index >= end - start) {
        throw new IndexOutOfBoundsException(index);
      }
      return parent.getItemAsLong(index - start);
    }

    @Override
    public ColumnLongStorageIterator iteratorWithIndex() {
      return new LongStorageIterator(parent, start, getSize());
    }
  }

  private static class SliceDoubleStorage extends SliceStorage<Double>
      implements ColumnDoubleStorage {
    private final ColumnDoubleStorage parent;

    public SliceDoubleStorage(ColumnDoubleStorage parent, long start, long end) {
      super(parent, start, end);
      this.parent = parent;
    }

    @Override
    public FloatType getType() {
      return parent.getType();
    }

    @Override
    public double getItemAsDouble(long index) throws ValueIsNothingException {
      if (index < 0 || index >= end - start) {
        throw new IndexOutOfBoundsException(index);
      }
      return parent.getItemAsDouble(index - start);
    }

    @Override
    public ColumnDoubleStorageIterator iteratorWithIndex() {
      return new DoubleStorageIterator(parent, start, getSize());
    }
  }

  private static class SliceBooleanStorage extends SliceStorage<Boolean>
      implements ColumnBooleanStorage {
    private final ColumnBooleanStorage parent;

    public SliceBooleanStorage(ColumnBooleanStorage parent, long start, long end) {
      super(parent, start, end);
      this.parent = parent;
    }

    @Override
    public boolean getItemAsBoolean(long index) throws ValueIsNothingException {
      if (index < 0 || index >= end - start) {
        throw new IndexOutOfBoundsException(index);
      }
      return parent.getItemAsBoolean(index - start);
    }

    @Override
    public ColumnBooleanStorageIterator iteratorWithIndex() {
      return new BooleanStorageIterator(parent, start, getSize());
    }
  }
}
