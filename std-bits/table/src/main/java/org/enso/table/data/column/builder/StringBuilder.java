package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;

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
    String str = (String) o;
    if (!type.fits(str)) {
      // TODO
      throw new IllegalArgumentException("String does not fit the type");
    }

    data[currentSize++] = str;
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
  protected Storage<String> doSeal() {
    return new StringStorage(data, currentSize, type);
  }
}
