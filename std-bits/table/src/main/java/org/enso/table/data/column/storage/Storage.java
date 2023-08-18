package org.enso.table.data.column.storage;

import java.util.BitSet;
import java.util.HashMap;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.InferredBuilder;
import org.enso.table.data.column.builder.MixedBuilder;
import org.enso.table.data.column.operation.cast.CastProblemBuilder;
import org.enso.table.data.column.operation.cast.StorageConverter;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

/** An abstract representation of a data column. */
public abstract class Storage<T> {
  /** @return the number of elements in this column (including NAs) */
  public abstract int size();

  /** @return the number of NA elements in this column */
  public abstract int countMissing();

  /** @return the type tag of this column's storage. */
  public abstract StorageType getType();

  /**
   * @return the type of the values in this column's storage. Most storages just return their type.
   *     Mixed storage will try to see if all elements fit some more precise type.
   */
  public StorageType inferPreciseType() {
    return getType();
  }

  /**
   * Returns a more specialized storage, if available.
   *
   * <p>This storage should have the same type as returned by {@code inferPreciseType}. See {@link
   * MixedStorage} for more information.
   */
  public Storage<?> tryGettingMoreSpecializedStorage() {
    return this;
  }

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
  public abstract T getItemBoxed(int idx);

  /** A container for names of vectorizable operation. */
  public static final class Maps {
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
    public static final String POWER = "^";
    public static final String TRUNCATE = "truncate";
    public static final String CEIL = "ceil";
    public static final String FLOOR = "floor";
    public static final String ROUND = "round";
    public static final String NOT = "not";
    public static final String AND = "&&";
    public static final String OR = "||";
    public static final String IS_NOTHING = "is_nothing";
    public static final String IS_NAN = "is_nan";
    public static final String IS_INFINITE = "is_infinite";
    public static final String IS_EMPTY = "is_empty";
    public static final String STARTS_WITH = "starts_with";
    public static final String ENDS_WITH = "ends_with";
    public static final String CONTAINS = "contains";
    public static final String LIKE = "like";
    public static final String IS_IN = "is_in";
    public static final String YEAR = "year";
    public static final String QUARTER = "quarter";
    public static final String MONTH = "month";
    public static final String WEEK = "week";
    public static final String DAY = "day";
    public static final String HOUR = "hour";
    public static final String MINUTE = "minute";
    public static final String SECOND = "second";
    public static final String MILLISECOND = "millisecond";
    public static final String MICROSECOND = "microsecond";
    public static final String NANOSECOND = "nanosecond";
  }

  /* Specifies if the given unary operation has a vectorized implementation available for this storage.*/
  public abstract boolean isUnaryOpVectorized(String name);

  /** Runs a vectorized unary operation. */
  public abstract Storage<?> runVectorizedUnaryMap(
      String name, MapOperationProblemBuilder problemBuilder);

  /* Specifies if the given binary operation has a vectorized implementation available for this storage.*/
  public abstract boolean isBinaryOpVectorized(String name);

  /** Runs a vectorized operation on this storage, taking one scalar argument. */
  public abstract Storage<?> runVectorizedBinaryMap(
      String name, Object argument, MapOperationProblemBuilder problemBuilder);

  /* Specifies if the given ternary operation has a vectorized implementation available for this storage.*/
  public boolean isTernaryOpVectorized(String name) {
    return false;
  }

  /** Runs a vectorized operation on this storage, taking two scalar arguments. */
  public Storage<?> runVectorizedTernaryMap(
      String name, Object argument0, Object argument1, MapOperationProblemBuilder problemBuilder) {
    throw new IllegalArgumentException("Unsupported ternary operation: " + name);
  }

  /**
   * Runs a vectorized operation on this storage, taking a storage as the right argument -
   * processing row-by-row.
   */
  public abstract Storage<?> runVectorizedZip(
      String name, Storage<?> argument, MapOperationProblemBuilder problemBuilder);

  /**
   * Runs a unary function on each non-null element in this storage.
   *
   * @param function the function to run.
   * @param skipNa whether rows containing missing values should be passed to the function.
   * @param expectedResultType the expected type for the result storage; it is ignored if the
   *     operation is vectorized
   * @return the result of running the function on each row
   */
  public final Storage<?> unaryMap(
      Function<Object, Value> function, boolean skipNa, StorageType expectedResultType) {
    Builder storageBuilder = Builder.getForType(expectedResultType, size());
    Context context = Context.getCurrent();
    for (int i = 0; i < size(); i++) {
      Object it = getItemBoxed(i);
      if (skipNa && it == null) {
        storageBuilder.appendNulls(1);
      } else {
        Value result = function.apply(it);
        Object converted = Polyglot_Utils.convertPolyglotValue(result);
        storageBuilder.appendNoGrow(converted);
      }

      context.safepoint();
    }
    return storageBuilder.seal();
  }

  /**
   * Runs a 2-argument function on each element in this storage.
   *
   * @param function the function to run.
   * @param argument the argument to pass to each run of the function
   * @param skipNulls specifies whether null values on the input should result in a null result
   *     without passing them through the function, this is useful if the function does not support
   *     the null-values, but it needs to be set to false if the function should handle them.
   * @param expectedResultType the expected type for the result storage
   * @return a new storage containing results of the function for each row
   */
  public final Storage<?> binaryMap(
      BiFunction<Object, Object, Object> function,
      Object argument,
      boolean skipNulls,
      StorageType expectedResultType) {
    Builder storageBuilder = Builder.getForType(expectedResultType, size());
    if (skipNulls && argument == null) {
      storageBuilder.appendNulls(size());
      return storageBuilder.seal();
    }

    Context context = Context.getCurrent();
    for (int i = 0; i < size(); i++) {
      Object it = getItemBoxed(i);
      if (skipNulls && it == null) {
        storageBuilder.appendNulls(1);
      } else {
        Object result = function.apply(it, argument);
        Object converted = Polyglot_Utils.convertPolyglotValue(result);
        storageBuilder.appendNoGrow(converted);
      }

      context.safepoint();
    }
    return storageBuilder.seal();
  }

  /**
   * Runs a function on each pair of non-missing elements in this and arg.
   *
   * @param function the function to run.
   * @param skipNa whether rows containing missing values should be passed to the function.
   * @param expectedResultType the expected type for the result storage; it is ignored if the
   *     operation is vectorized
   * @return the result of running the function on all non-missing elements.
   */
  public final Storage<?> zip(
      BiFunction<Object, Object, Object> function,
      Storage<?> arg,
      boolean skipNa,
      StorageType expectedResultType) {
    Builder storageBuilder = Builder.getForType(expectedResultType, size());
    Context context = Context.getCurrent();
    for (int i = 0; i < size(); i++) {
      Object it1 = getItemBoxed(i);
      Object it2 = i < arg.size() ? arg.getItemBoxed(i) : null;
      if (skipNa && (it1 == null || it2 == null)) {
        storageBuilder.appendNulls(1);
      } else {
        Object result = function.apply(it1, it2);
        Object converted = Polyglot_Utils.convertPolyglotValue(result);
        storageBuilder.appendNoGrow(converted);
      }

      context.safepoint();
    }
    return storageBuilder.seal();
  }

  /**
   * Runs a unary operation.
   *
   * <p>If a vectorized implementation is available, it is used, otherwise the fallback is used.
   *
   * @param name the name of the vectorized operation
   * @param problemBuilder the problem builder to use for the vectorized implementation
   * @param fallback the fallback Enso function to run if vectorized implementation is not
   *     available; it should never raise dataflow errors.
   * @param skipNa whether rows containing missing values should be passed to the fallback function.
   * @param expectedResultType the expected type for the result storage; it is ignored if the
   *     operation is vectorized
   * @return the result of running the operation on each row
   */
  public final Storage<?> vectorizedOrFallbackUnaryMap(
      String name,
      MapOperationProblemBuilder problemBuilder,
      Function<Object, Value> fallback,
      boolean skipNa,
      StorageType expectedResultType) {
    if (isUnaryOpVectorized(name)) {
      return runVectorizedUnaryMap(name, problemBuilder);
    } else {
      checkFallback(fallback, expectedResultType, name);
      return unaryMap(fallback, skipNa, expectedResultType);
    }
  }

  /**
   * Runs a binary operation with a scalar argument.
   *
   * <p>If a vectorized implementation is available, it is used, otherwise the fallback is used.
   *
   * @param name the name of the vectorized operation
   * @param problemBuilder the problem builder to use for the vectorized implementation
   * @param fallback the fallback Enso function to run if vectorized implementation is not
   *     available; it should never raise dataflow errors.
   * @param argument the argument to pass to each run of the function
   * @param skipNulls specifies whether null values on the input should result in a null result
   * @param expectedResultType the expected type for the result storage; it is ignored if the
   *     operation is vectorized
   * @return the result of running the operation on each row
   */
  public final Storage<?> vectorizedOrFallbackBinaryMap(
      String name,
      MapOperationProblemBuilder problemBuilder,
      BiFunction<Object, Object, Object> fallback,
      Object argument,
      boolean skipNulls,
      StorageType expectedResultType) {
    if (isBinaryOpVectorized(name)) {
      return runVectorizedBinaryMap(name, argument, problemBuilder);
    } else {
      checkFallback(fallback, expectedResultType, name);
      return binaryMap(fallback, argument, skipNulls, expectedResultType);
    }
  }

  /**
   * Runs a ternary operation with two scalar arguments.
   *
   * <p>Does not take a fallback function.
   *
   * @param name the name of the vectorized operation
   * @param problemBuilder the problem builder to use for the vectorized implementation
   * @param argument0 the first argument to pass to each run of the function
   * @param argument1 the second argument to pass to each run of the function
   * @param skipNulls specifies whether null values on the input should result in a null result
   * @param expectedResultType the expected type for the result storage; it is ignored if the
   *     operation is vectorized
   * @return the result of running the operation on each row
   */
  public final Storage<?> vectorizedTernaryMap(
      String name,
      MapOperationProblemBuilder problemBuilder,
      Object argument0,
      Object argument1,
      boolean skipNulls,
      StorageType expectedResultType) {
    if (isTernaryOpVectorized(name)) {
      return runVectorizedTernaryMap(name, argument0, argument1, problemBuilder);
    } else {
      throw new IllegalArgumentException("Unsupported ternary operation: " + name);
    }
  }

  /**
   * Runs a binary operation with a storage argument.
   *
   * <p>If a vectorized implementation is available, it is used, otherwise the fallback is used.
   *
   * @param name the name of the vectorized operation
   * @param problemBuilder the problem builder to use for the vectorized implementation
   * @param fallback the fallback Enso function to run if vectorized implementation is not
   *     available; it should never raise dataflow errors.
   * @param other the other storage to zip with this one
   * @param skipNulls specifies whether null values on the input should result in a null result
   * @param expectedResultType the expected type for the result storage; it is ignored if the
   *     operation is vectorized
   * @return the result of running the operation on each row
   */
  public final Storage<?> vectorizedOrFallbackZip(
      String name,
      MapOperationProblemBuilder problemBuilder,
      BiFunction<Object, Object, Object> fallback,
      Storage<?> other,
      boolean skipNulls,
      StorageType expectedResultType) {
    if (isBinaryOpVectorized(name)) {
      return runVectorizedZip(name, other, problemBuilder);
    } else {
      checkFallback(fallback, expectedResultType, name);
      return zip(fallback, other, skipNulls, expectedResultType);
    }
  }

  private void checkFallback(Object fallback, StorageType storageType, String operationName)
      throws IllegalArgumentException {
    if (fallback == null) {
      if (operationName == null) {
        throw new IllegalArgumentException(
            "A function or name of vectorized operation must be specified. This is a bug in the Table library.");
      } else {
        String className = this.getClass().getName();
        throw new IllegalArgumentException(
            "The operation "
                + operationName
                + " has no vectorized implementation for "
                + className
                + ", but no fallback function was provided. This is a bug in the Table library.");
      }
    }

    if (storageType == null) {
      throw new IllegalArgumentException(
          "The expected result type must be specified if a fallback function is used. This is a bug in the Table library.");
    }
  }

  /**
   * Return a new storage, where missing elements have been replaced by arg.
   *
   * @param arg the value to use for missing elements
   * @return a new storage, with all missing elements replaced by arg
   */
  public Storage<?> fillMissing(Value arg) {
    return fillMissingHelper(arg, new MixedBuilder(size()));
  }

  /**
   * Fills missing values in this storage, by using corresponding values from {@code other}.
   *
   * @param other the source of default values
   * @param commonType a common type that should fit values from both storages
   * @return a new storage with missing values filled
   */
  public Storage<?> fillMissingFrom(Storage<?> other, StorageType commonType) {
    var builder = Builder.getForType(commonType, size());
    Context context = Context.getCurrent();
    for (int i = 0; i < size(); i++) {
      if (isNa(i)) {
        builder.appendNoGrow(other.getItemBoxed(i));
      } else {
        builder.appendNoGrow(getItemBoxed(i));
      }

      context.safepoint();
    }
    return builder.seal();
  }

  protected final Storage<?> fillMissingHelper(Value arg, Builder builder) {
    Object convertedFallback = Polyglot_Utils.convertPolyglotValue(arg);
    Context context = Context.getCurrent();
    for (int i = 0; i < size(); i++) {
      Object it = getItemBoxed(i);
      if (it == null) {
        builder.appendNoGrow(convertedFallback);
      } else {
        builder.appendNoGrow(it);
      }

      context.safepoint();
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
  public abstract Storage<T> mask(BitSet mask, int cardinality);

  /**
   * Returns a new storage, ordered according to the rules specified in a mask.
   *
   * @param mask@return a storage resulting from applying the reordering rules
   */
  public abstract Storage<T> applyMask(OrderMask mask);

  /**
   * Returns a new storage, resulting from applying the rules specified in a mask. The resulting
   * storage should contain the elements of the original storage, in the same order. However, the
   * number of consecutive copies of the i-th element of the original storage should be {@code
   * counts[i]}.
   *
   * @param counts the mask specifying elements duplication
   * @param total the sum of all elements in the mask, also interpreted as the length of the
   *     resulting storage
   * @return the storage masked according to the specified rules
   */
  public abstract Storage<T> countMask(int[] counts, int total);

  /** @return a copy of the storage containing a slice of the original data */
  public abstract Storage<T> slice(int offset, int limit);

  /**
   * @return a new storage instance, containing the same elements as this one, with {@code count}
   *     nulls appended at the end
   */
  public Storage<?> appendNulls(int count) {
    Builder builder = new InferredBuilder(size() + count);
    builder.appendBulkStorage(this);
    builder.appendNulls(count);
    return builder.seal();
  }

  /**
   * Creates a builder that is capable of creating storages of the same type as the current one.
   *
   * <p>This is useful for example when copying the current storage with some modifications.
   */
  public abstract Builder createDefaultBuilderOfSameType(int capacity);

  /** @return a copy of the storage consisting of slices of the original data */
  public abstract Storage<T> slice(List<SliceRange> ranges);

  public List<Object> toList() {
    return new StorageListView(this);
  }

  /**
   * Counts the number of times each value has been seen before in this storage.
   *
   * @return a storage counting the number of times each value in this one has been seen before.
   */
  public Storage<?> duplicateCount() {
    long[] data = new long[size()];
    HashMap<Object, Integer> occurenceCount = new HashMap<>();
    Context context = Context.getCurrent();
    for (int i = 0; i < size(); i++) {
      var value = getItemBoxed(i);
      var count = occurenceCount.getOrDefault(value, 0);
      data[i] = count;
      occurenceCount.put(value, count + 1);
      context.safepoint();
    }
    return new LongStorage(data);
  }

  public final Storage<?> cast(StorageType targetType, CastProblemBuilder castProblemBuilder) {
    StorageConverter<?> converter = StorageConverter.fromStorageType(targetType);
    return converter.cast(this, castProblemBuilder);
  }
}
