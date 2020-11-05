package org.enso.table.data.column;

/** An abstract representation of a data column. */
public abstract class Storage {
  /** @return the number of elements in this column (including NAs) */
  public abstract long size();

  /** @return the type tag of this column's storage. Must be one of {@link Type} */
  public abstract long getType();

  /**
   * Checks whether the value at {@code idx} is missing.
   *
   * @param idx the index to check.
   * @return whether or not the value is missing.
   */
  public abstract boolean isNa(long idx);

  /**
   * Enumerating possible storage types.
   *
   * <p>Keep in sync with variables in {@code Table.Table}. These variables are copied between Enso
   * and Java code, in order to make them trivially constant on the Enso side, without invoking the
   * polyglot machinery to access them.
   */
  public static final class Type {
    public static final long LONG = 1;
    public static final long DOUBLE = 2;
    public static final long STRING = 3;
  }
}
