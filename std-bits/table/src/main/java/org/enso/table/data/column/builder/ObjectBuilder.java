package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.ObjectStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.NullType;

/** A builder for boxed object columns. */
public class ObjectBuilder extends TypedBuilder<Object> {
  public ObjectBuilder(int size) {
    super(AnyObjectType.INSTANCE, new Object[size]);
  }

  @Override
  public boolean accepts(Object o) {
    return true;
  }

  @Override
  public void append(Object o) {
    ensureSpaceToAppend();
    data[currentSize++] = o;
  }

  @Override
  public void appendBulkStorage(Storage<?> storage) {
    if (currentSize + storage.size() > data.length) {
      resize(currentSize + storage.size());
    }

    if (storage instanceof SpecializedStorage<?> specializedStorage) {
      System.arraycopy(specializedStorage.getData(), 0, data, currentSize, storage.size());
      currentSize += storage.size();
    } else if (storage.getType() instanceof NullType) {
      appendNulls(storage.size());
    } else {
      int n = storage.size();
      for (int i = 0; i < n; i++) {
        data[currentSize++] = storage.getItemBoxed(i);
      }
    }
  }

  @Override
  public Storage<Object> doSeal() {
    return new ObjectStorage(data, currentSize);
  }
}
