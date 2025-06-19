package org.enso.table.data.column.storage;

import java.util.BitSet;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;

/** An abstract representation of a data column. */
public abstract class Storage<T> implements ColumnStorage<T> {
  @Override
  public abstract long getSize();

  @Override
  public abstract StorageType<T> getType();

  @Override
  public abstract boolean isNothing(long index);

  @Override
  public abstract T getItemBoxed(long index);

  @Override
  public Iterator<T> iterator() {
    return new Iterator<>() {
      private long index = -1;

      @Override
      public boolean hasNext() {
        return index + 1 < getSize();
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

  @Override
  public ColumnStorageIterator<T> iteratorWithIndex() {
    return new StorageIterator<>(this);
  }

  public static class StorageIterator<T> implements ColumnStorageIterator<T> {
    protected final ColumnStorage<T> parent;
    protected long index = -1;

    public StorageIterator(ColumnStorage<T> parent) {
      this.parent = parent;
    }

    @Override
    public T getItemBoxed() {
      return parent.getItemBoxed(index);
    }

    @Override
    public boolean isNothing() {
      return parent.isNothing(index);
    }

    @Override
    public boolean hasNext() {
      return index + 1 < parent.getSize();
    }

    @Override
    public T next() {
      if (!hasNext()) {
        throw new NoSuchElementException();
      }
      return parent.getItemBoxed(++index);
    }

    @Override
    public long getIndex() {
      return index;
    }

    @Override
    public boolean moveNext() {
      if (!hasNext()) {
        return false;
      }
      index++;
      return true;
    }
  }

  /**
   * Return a new storage, containing only the items marked true in the mask.
   *
   * @param filterMask the mask to use
   * @param newLength the number of true values in mask
   * @return a new storage, filtered with the given mask
   */
  public abstract ColumnStorage<T> applyFilter(BitSet filterMask, int newLength);

  /**
   * Returns a new storage, ordered according to the rules specified in a mask.
   *
   * @param mask@return a storage resulting from applying the reordering rules
   */
  public abstract ColumnStorage<T> applyMask(OrderMask mask);

  /**
   * @return a copy of the storage containing a slice of the original data
   */
  public abstract ColumnStorage<T> slice(int offset, int limit);

  /**
   * @return a copy of the storage consisting of slices of the original data
   */
  public abstract ColumnStorage<T> slice(List<SliceRange> ranges);
}
