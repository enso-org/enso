package org.enso.table.data.column.storage;

import org.enso.table.data.column.storage.type.TextType;

/** A column storing strings. */
public final class StringStorage extends SpecializedStorage<String> {
  /**
   * @param data the underlying data
   * @param type the type of the column
   */
  public StringStorage(String[] data, TextType type) {
    super(type, data);
  }

  @Override
  public TextType getType() {
    // As the type is fixed, we can safely cast it.
    return (TextType) super.getType();
  }

  @Override
  protected SpecializedStorage<String> newInstance(String[] data) {
    return new StringStorage(data, getType());
  }

  @Override
  protected String[] newUnderlyingArray(int size) {
    return new String[size];
  }
}
