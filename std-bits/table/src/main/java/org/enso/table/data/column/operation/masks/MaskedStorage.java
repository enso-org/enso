package org.enso.table.data.column.operation.masks;

import java.util.Iterator;
import java.util.NoSuchElementException;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.Storage;

public class MaskedStorage<T> extends Storage<T> {
  private final ColumnStorage<T> parent;
  private final IndexMapper indexMapper;

  MaskedStorage(ColumnStorage<T> parent, IndexMapper indexMapper) {
    super(parent.getType());
    this.parent = parent;
    this.indexMapper = indexMapper;
  }

  public ColumnStorage<T> parent() {
    return parent;
  }

  public IndexMapper indexMapper() {
    return indexMapper;
  }

  protected long mapIndex(long index) {
    if (index < 0 || index >= indexMapper.size()) {
      throw new IndexOutOfBoundsException(index);
    }
    return indexMapper.map(index);
  }

  @Override
  public long getSize() {
    return indexMapper.size();
  }

  @Override
  public boolean isNothing(long index) {
    var mappedIndex = mapIndex(index);
    return mappedIndex == IndexMapper.NOT_FOUND_INDEX || parent.isNothing(mappedIndex);
  }

  @Override
  public T getItemBoxed(long index) {
    var mappedIndex = mapIndex(index);
    return mappedIndex == IndexMapper.NOT_FOUND_INDEX ? null : parent.getItemBoxed(mappedIndex);
  }

  @Override
  public Iterator<T> iterator() {
    return new Iterator<>() {
      private long index = -1;

      @Override
      public boolean hasNext() {
        return index + 1 < indexMapper.size();
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
