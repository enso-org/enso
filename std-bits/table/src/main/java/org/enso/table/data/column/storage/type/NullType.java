package org.enso.table.data.column.storage.type;

public record NullType() implements StorageType {
  public static final NullType INSTANCE = new NullType();

  @Override
  public boolean isNumeric() {
    return true;
  }

  @Override
  public boolean hasDate() {
    return true;
  }

  @Override
  public boolean hasTime() {
    return true;
  }
}
