package org.enso.table.data.column.storage;

import java.lang.reflect.Array;
import java.util.Collection;
import java.util.List;
import java.util.ListIterator;
import java.util.Objects;

/**
 * Wraps a storage in a list. Used for exposing a polyglot array interface back to Enso. This list
 * is not modifiable.
 */
public class StorageListView implements List<Object> {
  private final Storage storage;
  private final int from;
  private final int to;

  /**
   * Wraps a storage in an instance of this.
   *
   * @param storage the storage to wrap.
   */
  public StorageListView(Storage storage) {
    this(storage, 0, storage.size());
  }

  private StorageListView(Storage storage, int from, int to) {
    this.storage = storage;
    this.from = from;
    this.to = to;
  }

  @Override
  public boolean add(Object o) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void add(int index, Object element) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean addAll(Collection<?> c) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean addAll(int index, Collection<?> c) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void clear() {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean contains(Object o) {
    return indexOf(o) != -1;
  }

  @Override
  public boolean containsAll(Collection<?> c) {
    for (Object o : c) {
      if (!contains(o)) {
        return false;
      }
    }
    return true;
  }

  @SuppressWarnings("rawtypes")
  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof List that)) {
      return false;
    }
    if (that.size() != size()) {
      return false;
    }
    for (int i = 0; i < size(); i++) {
      if (!Objects.equals(get(i), that.get(i))) {
        return false;
      }
    }
    return true;
  }

  @Override
  public Object get(int index) {
    return storage.getItemBoxed(from + index);
  }

  @Override
  public int hashCode() {
    int hashCode = 1;
    for (Object e : this) {
      hashCode = 31 * hashCode + (e == null ? 0 : e.hashCode());
    }
    return hashCode;
  }

  @Override
  public int indexOf(Object o) {
    for (int i = 0; i < size(); i++) {
      if (Objects.equals(o, get(i))) {
        return i;
      }
    }
    return -1;
  }

  @Override
  public int lastIndexOf(Object o) {
    for (int i = size() - 1; i >= 0; i--) {
      if (Objects.equals(o, get(i))) {
        return i;
      }
    }
    return -1;
  }

  @Override
  public Object set(int index, Object element) {
    throw new UnsupportedOperationException();
  }

  @Override
  public int size() {
    return to - from;
  }

  @Override
  public Object remove(int index) {
    throw new UnsupportedOperationException();
  }

  @Override
  public ListIterator<Object> listIterator() {
    return listIterator(0);
  }

  @Override
  public ListIterator<Object> listIterator(int index) {
    return new Iterator(this, 0);
  }

  @Override
  public List<Object> subList(int fromIndex, int toIndex) {
    if (fromIndex < 0 || toIndex > size() || fromIndex > toIndex) {
      throw new IndexOutOfBoundsException();
    }
    return new StorageListView(storage, from + fromIndex, from + toIndex);
  }

  @Override
  public boolean isEmpty() {
    return size() > 0;
  }

  @Override
  public java.util.Iterator<Object> iterator() {
    return listIterator();
  }

  @Override
  public Object[] toArray() {
    return toArray(new Object[0]);
  }

  @Override
  @SuppressWarnings("unchecked")
  public <T> T[] toArray(T[] a) {
    T[] result =
        a.length >= size() ? a : (T[]) Array.newInstance(a.getClass().getComponentType(), size());
    for (int i = 0; i < size(); i++) {
      result[i] = (T) get(i);
    }
    return result;
  }

  @Override
  public boolean remove(Object o) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean removeAll(Collection<?> c) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean retainAll(Collection<?> c) {
    throw new UnsupportedOperationException();
  }

  private static class Iterator implements ListIterator<Object> {
    private final List<Object> list;
    private int position;

    private Iterator(List<Object> list, int position) {
      this.list = list;
      this.position = position;
    }

    @Override
    public boolean hasNext() {
      return position < list.size();
    }

    @Override
    public Object next() {
      return list.get(position++);
    }

    @Override
    public boolean hasPrevious() {
      return position > 0;
    }

    @Override
    public Object previous() {
      return list.get(--position);
    }

    @Override
    public int nextIndex() {
      return position;
    }

    @Override
    public int previousIndex() {
      return position - 1;
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException();
    }

    @Override
    public void set(Object o) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void add(Object o) {
      throw new UnsupportedOperationException();
    }
  }
}
