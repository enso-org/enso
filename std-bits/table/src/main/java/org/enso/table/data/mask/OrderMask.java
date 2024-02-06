package org.enso.table.data.mask;

import java.util.function.ToIntFunction;

/** Describes a storage reordering operator. */
public interface OrderMask {
  int length();

  /**
   * Describes the reordering that should happen on the applying storage at the index.
   *
   * <p>The resulting storage should contain the {@code positions[i]}-th element of the original
   * storage at the {@code idx}-th position. It may return {@link
   * org.enso.table.data.index.Index.NOT_FOUND}, in which case a missing value should be inserted at
   * this position.
   */
  int get(int idx);

  static OrderMask empty() {
    return new OrderMaskFromArray(new int[0], 0);
  }

  static OrderMask reverse(int size) {
    return new OrderMaskReversed(size);
  }

  static OrderMask fromArray(int[] positions) {
    return fromArray(positions, positions.length);
  }

  static OrderMask fromArray(int[] positions, int length) {
    return new OrderMaskFromArray(positions, length);
  }

  static <T> OrderMask fromObjects(T[] input, ToIntFunction<T> function) {
    return new OrderMaskGeneric<>(input, function);
  }

  class OrderMaskFromArray implements OrderMask {
    private final int[] positions;
    private final int length;

    public OrderMaskFromArray(int[] positions, int length) {
      this.positions = positions;
      this.length = length;
    }

    @Override
    public int length() {
      return length;
    }

    @Override
    public int get(int idx) {
      return positions[idx];
    }
  }

  class OrderMaskGeneric<T> implements OrderMask {
    private final T[] positions;
    private final ToIntFunction<T> function;

    public OrderMaskGeneric(T[] positions, ToIntFunction<T> function) {
      this.positions = positions;
      this.function = function;
    }

    @Override
    public int length() {
      return positions.length;
    }

    @Override
    public int get(int idx) {
      return function.applyAsInt(positions[idx]);
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
    public int get(int idx) {
      return length - idx - 1;
    }
  }
}
