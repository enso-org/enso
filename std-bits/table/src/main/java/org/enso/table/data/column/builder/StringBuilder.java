package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.error.ValueTypeMismatchException;

/** A builder for string columns. */
public class StringBuilder extends TypedBuilderImpl<String> {
  private final TextType type;

  @Override
  protected String[] newArray(int size) {
    return new String[size];
  }

  public StringBuilder(int size, TextType type) {
    super(size);
    this.type = type;
  }

  @Override
  public StorageType getType() {
    return type;
  }

  @Override
  public void appendNoGrow(Object o) {
    try {
      String str = (String) o;
      if (type.fits(str)) {
        data[currentSize++] = str;
      } else {
        throw new ValueTypeMismatchException(type, str);
      }
    } catch (ClassCastException e) {
      throw new ValueTypeMismatchException(type, o);
    }
  }

  @Override
  public boolean accepts(Object o) {
    if (o instanceof String s) {
      return type.fits(s);
    } else {
      return false;
    }
  }

  @Override
  public void appendBulkStorage(Storage<?> storage) {
    if (storage.getType() instanceof TextType gotType) {
      if (type.fitsExactly(gotType)) {
        if (storage instanceof SpecializedStorage<?>) {
          // This cast is safe, because storage.getType() == this.getType() == TextType iff
          // storage.T == String
          @SuppressWarnings("unchecked")
          SpecializedStorage<String> specializedStorage = (SpecializedStorage<String>) storage;
          System.arraycopy(specializedStorage.getData(), 0, data, currentSize, storage.size());
          currentSize += storage.size();
          return;
        }
      }
    }

    super.appendBulkStorage(storage);
  }

  @Override
  protected Storage<String> doSeal() {
    return new StringStorage(data, currentSize, type);
  }
}
