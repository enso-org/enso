package org.enso.table.data.column.storage;

import java.util.BitSet;
import java.util.function.BiFunction;
import java.util.function.Function;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.InferredBuilder;
import org.enso.table.data.column.builder.object.ObjectBuilder;

/** An abstract representation of a data column. */
public abstract class Storage {
  /** @return the number of elements in this column (including NAs) */
  public abstract int size();

  /** @return the number of NA elements in this column */
  public abstract int countMissing();

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
    public static final String LT = "<";
    public static final String LTE = "<=";
    public static final String GT = ">";
    public static final String GTE = ">=";
    public static final String MUL = "*";
    public static final String ADD = "+";
    public static final String SUB = "-";
    public static final String DIV = "/";
    public static final String MOD = "%";
    public static final String NOT = "not";
    public static final String AND = "&&";
    public static final String OR = "||";
    public static final String IS_MISSING = "is_missing";
    public static final String STARTS_WITH = "starts_with";
    public static final String ENDS_WITH = "ends_with";
    public static final String CONTAINS = "contains";
  }

  protected abstract boolean isOpVectorized(String name);

  protected abstract Storage runVectorizedMap(String name, Object argument);

  protected abstract Storage runVectorizedZip(String name, Storage argument);

  /**
   * Runs a function on each non-missing element in this storage and gathers the results.
   *
   * @param name a name of potential vectorized variant of the function that should be used if
   *     supported. If this argument is null, the vectorized operation will never be used.
   * @param function the function to run.
   * @param argument the argument to pass to each run of the function
   * @return the result of running the function on all non-missing elements.
   */
  public final Storage bimap(
      String name, BiFunction<Object, Object, Object> function, Object argument) {
    if (name != null && isOpVectorized(name)) {
      return runVectorizedMap(name, argument);
    }
    Builder builder = new InferredBuilder(size());
    for (int i = 0; i < size(); i++) {
      Object it = getItemBoxed(i);
      if (it == null) {
        builder.append(null);
      } else {
        builder.append(function.apply(it, argument));
      }
    }
    return builder.seal();
  }

  /**
   * Runs a function on each non-missing element in this storage and gathers the results.
   *
   * @param name a name of potential vectorized variant of the function that should be used if
   *     supported. If this argument is null, the vectorized operation will never be used.
   * @param function the function to run.
   * @return the result of running the function on all non-missing elements.
   */
  public final Storage map(String name, Function<Object, Object> function) {
    if (name != null && isOpVectorized(name)) {
      return runVectorizedMap(name, null);
    }
    Builder builder = new InferredBuilder(size());
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

  /**
   * Runs a function on each pair of non-missing elements in this and arg.
   *
   * @param name a name of potential vectorized variant of the function that should be used if
   *     supported. If this argument is null, the vectorized operation will never be used. *
   * @param function the function to run.
   * @return the result of running the function on all non-missing elements.
   */
  public final Storage zip(String name, BiFunction<Object, Object, Object> function, Storage arg) {
    if (name != null && isOpVectorized(name)) {
      return runVectorizedZip(name, arg);
    }
    Builder builder = new InferredBuilder(size());
    for (int i = 0; i < size(); i++) {
      Object it1 = getItemBoxed(i);
      Object it2 = i < arg.size() ? arg.getItemBoxed(i) : null;
      if (it1 == null || it2 == null) {
        builder.append(null);
      } else {
        builder.append(function.apply(it1, it2));
      }
    }
    return builder.seal();
  }

  /**
   * Return a new storage, where missing elements have been replaced by arg.
   *
   * @param arg the value to use for missing elements
   * @return a new storage, with all missing elements replaced by arg
   */
  public Storage fillMissing(Object arg) {
    return fillMissingHelper(arg, new ObjectBuilder(size()));
  }

  protected final Storage fillMissingHelper(Object arg, Builder builder) {
    for (int i = 0; i < size(); i++) {
      Object it = getItemBoxed(i);
      if (it == null) {
        builder.append(arg);
      } else {
        builder.append(it);
      }
    }
    return builder.seal();
  }

  /**
   * Return a new storage, containing only the items marked true in the mask.
   *
   * @param mask the mask to use
   * @param cardinality the number of true values in mask
   * @return a new storage, masked with the given mask
   */
  public abstract Storage mask(BitSet mask, int cardinality);

  /**
   * Returns a new storage, ordered according to the rules specified in a mask. The resulting
   * storage should contain the {@code positions[i]}-th element of the original storage at the i-th
   * position. {@code positions[i]} may be equal to {@link
   * org.enso.table.data.index.Index.NOT_FOUND}, in which case a missing value should be inserted at
   * this position.
   *
   * @param positions an array specifying the ordering as described
   * @return a storage resulting from applying the reordering rules
   */
  public abstract Storage orderMask(int[] positions);

  /**
   * Returns a new storage, resulting from applying the rules specified in a mask. The resulting
   * storage should contain the elements of the original storage, in the same order. However, the
   * number of consecutive copies of the i-th element of the original storage should be {@code
   * counts[i]}.
   *
   * @param counts the mask specifying elements duplication
   * @param total the sum of all elements in the mask, also interpreted as the size of the resulting
   *     storage
   * @return the storage masked according to the specified rules
   */
  public abstract Storage countMask(int[] counts, int total);
}
