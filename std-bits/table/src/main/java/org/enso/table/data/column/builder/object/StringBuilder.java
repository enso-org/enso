package org.enso.table.data.column.builder.object;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;

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
  public int getType() {
    return Storage.Type.STRING;
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
  public Storage<String> seal() {
    return new StringStorage(data, currentSize);
  }
}
