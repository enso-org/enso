package org.enso.table.data.mask;

/** Describes a storage reordering operator. */
public interface OrderMask {
  /** A constant representing the index of a missing value in a column. */
  int NOT_FOUND_INDEX = -1;

  int length();

  /**
   * Describes the reordering that should happen on the applying storage at the index.
   *
   * <p>The resulting storage should contain the {@code positions[i]}-th element of the original
   * storage at the {@code idx}-th position. It may return {@link NOT_FOUND_INDEX}, in which case a
   * missing value should be inserted at this position.
   *
   * <p>Indices may appear zero or multiple times in the mask - meaning rows that will be gone or
   * duplicated.
   */
  long get(int idx);

  static OrderMask reverse(int size) {
    return new OrderMaskReversed(size);
  }

  static OrderMask fromArray(long[] positions) {
    return new OrderMaskFromArray(positions);
  }

  class OrderMaskFromArray implements OrderMask {
    private final long[] positions;

    public OrderMaskFromArray(long[] positions) {
      this.positions = positions;
    }

    @Override
    public int length() {
      return positions.length;
    }

    @Override
    public long get(int idx) {
      return positions[idx];
    }

    @Override
    public long[] toLongArray() {
      return positions;
    }
  }

  class OrderMaskReversed implements OrderMask {
    private final int length;

    public OrderMaskReversed(int length) {
      this.length = length;
    }

    @Override
    public int length() {
      return length;
    }

    @Override
    public long get(int idx) {
      return length - idx - 1;
    }

    @Override
    public long[] toLongArray() {
      long[] result = new long[length];
      for (int i = 0; i < length; i++) {
        result[i] = length - i - 1;
      }
      return result;
    }
  }

  long[] toLongArray();
}
