package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;

/** A builder for string columns. */
public class StringBuilder extends TypedBuilderImpl<String> {
  @Override
  protected String[] newArray(int size) {
    return new String[size];
  }

  public StringBuilder(int size) {
    super(size);
  }

  @Override
  public StorageType getType() {
    return TextType.VARIABLE_LENGTH;
  }

  @Override
  public void appendNoGrow(Object o) {
    data[currentSize++] = (String) o;
  }

  @Override
  public boolean accepts(Object o) {
    return o instanceof String;
  }

  @Override
  protected Storage<String> doSeal() {
    return new StringStorage(data, currentSize);
  }
}
