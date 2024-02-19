package org.enso.table.data.column.storage.type;

public record BooleanType() implements StorageType {
  public static final BooleanType INSTANCE = new BooleanType();

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
