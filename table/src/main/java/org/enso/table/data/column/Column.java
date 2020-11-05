package org.enso.table.data.column;

public abstract class Column {
  public abstract long size();
  public abstract long getType();
  public abstract boolean isNa(long idx);

  /**
   * Enumerating possible storage types.
   *
   * Keep in sync with variables in {@code Table.Table}.
   */
  public static final class Type {
    public static final long LONG = 1;
    public static final long DOUBLE = 2;
    public static final long STRING = 3;
  }
}
