package org.enso.table.data.column.storage;

import org.enso.table.data.column.builder.object.InferredBuilder;

import java.util.BitSet;
import java.util.List;
import java.util.function.Function;

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
    public static final int LONG = 1;
    public static final int DOUBLE = 2;
    public static final int STRING = 3;
    public static final int BOOL = 4;
    public static final int OBJECT = 5;
  }

  public static final class Ops {
    public static final String EQ = "==";
  }

  public abstract boolean isOpVectorized(String name);

  public abstract Storage runVectorizedOp(String name, Object operand);

  public abstract Storage mask(BitSet mask, int cardinality);

  public abstract Storage map(Function<Object, Object> function);
}
