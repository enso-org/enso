package org.enso.table.data.column.storage.type;

public record AnyObjectType() implements StorageType {
  public static final AnyObjectType INSTANCE = new AnyObjectType();

  @Override
  public boolean isNumeric() {
    return false;
  }

  @Override
  public boolean hasDate() {
    return false;
  }

  @Override
  public boolean hasTime() {
    return false;
  }
}
