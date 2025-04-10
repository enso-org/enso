package org.enso.table.data.column.storage;

/**
 * Defines settings for inferring a possibly more precise type.
 *
 * @param shrinkIntegers specifies if the method should look for the smallest possible integer type that will fit the present values
 * @param wholeFloatsBecomeIntegers specifies if a float column that contains whole integers should be inferred as integer column
 */
public record PreciseTypeOptions(boolean shrinkIntegers, boolean wholeFloatsBecomeIntegers) {
  // The default setting that should be fast to compute, it may also rely on caching.
  static final PreciseTypeOptions DEFAULT =
      new PreciseTypeOptions(false, false);
}
