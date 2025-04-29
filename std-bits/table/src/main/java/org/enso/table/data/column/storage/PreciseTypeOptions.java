package org.enso.table.data.column.storage;

/**
 * Defines settings for inferring a possibly more precise type.
 *
 * @param shrinkIntegers specifies if the method should look for the smallest possible integer type
 *     that will fit the present values
 * @param shrinkText specifies if the method should look for the smallest possible text type that
 *     will fit the present values
 * @param wholeFloatsBecomeIntegers specifies if a float column that contains whole integers should
 *     be inferred as integer column
 */
public record PreciseTypeOptions(
    boolean shrinkIntegers, boolean shrinkText, boolean wholeFloatsBecomeIntegers) {
  /* The default setting that should be fast to compute. */
  public static final PreciseTypeOptions DEFAULT = new PreciseTypeOptions(false, false, false);

  public static final PreciseTypeOptions SHRINK = new PreciseTypeOptions(true, true, true);

  /** Returns options for the `auto_cast` operation. */
  public static PreciseTypeOptions forAutoCast(boolean shrinkTypes) {
    return new PreciseTypeOptions(shrinkTypes, shrinkTypes, true);
  }
}
