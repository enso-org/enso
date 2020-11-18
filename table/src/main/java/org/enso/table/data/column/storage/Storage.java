package org.enso.table.data.column.storage;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.InferredBuilder;

import java.util.BitSet;
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
   * Returns a boxed representation of an item. Missing values are denoted with null.
   *
   * @param idx the index to look up
   * @return the item at position {@code idx}
   */
  public abstract Object getItemBoxed(int idx);

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

  /** A container for names of vectorizable operation. */
  public static final class Ops {
    public static final String EQ = "==";
    public static final String NOT = "not";
  }

  /**
   * Checks whether a vectorized version of operation exists for this storage.
   *
   * @param name the operation name
   * @return whether a vectorized version is available
   */
  public abstract boolean isOpVectorized(String name);

  /**
   * Runs a vectorized operation on this storage. Can only be used if {@link
   * #isOpVectorized(String)} returns true.
   *
   * @param name the operation to run
   * @param operand an argument to the operation
   * @return the result of running operation over this storage
   */
  public abstract Storage runVectorizedOp(String name, Object operand);

  /**
   * Return a new storage, containing only the items marked true in the mask.
   *
   * @param mask the mask to use
   * @param cardinality the number of true values in mask
   * @return a new storage, masked with the given mask
   */
  public abstract Storage mask(BitSet mask, int cardinality);

  /**
   * Runs a function on each non-missing element in this storage and gathers the results.
   *
   * @param function the function to run.
   * @return the result of running the function on all non-missing elements.
   */
  public final Storage map(Function<Object, Object> function) {
    Builder builder = new InferredBuilder((int) size());
    for (int i = 0; i < size(); i++) {
      Object it = getItemBoxed(i);
      if (it == null) {
        builder.append(null);
      } else {
        builder.append(function.apply(it));
      }
    }
    return builder.seal();
  }
}
