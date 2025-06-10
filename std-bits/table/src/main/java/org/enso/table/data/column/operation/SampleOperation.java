package org.enso.table.data.column.operation;

// Base class for untrimmed and non-trivial whitespace counts
public class SampleOperation {

  // Default seed for random number generation (no specific reason for this value, just stability on
  // results).
  public static final long RANDOM_SEED = 677280131;

  // Default sample size for counting untrimmed cells.
  public static final long DEFAULT_SAMPLE_SIZE = 10000;
}
