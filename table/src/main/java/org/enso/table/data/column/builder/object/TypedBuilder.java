package org.enso.table.data.column.builder.object;

public abstract class TypedBuilder extends Builder {
  public abstract void writeTo(Object[] items);

  public abstract boolean canRetypeTo(long type);

  public abstract TypedBuilder retypeTo(long type);

  public abstract int getType();
}
