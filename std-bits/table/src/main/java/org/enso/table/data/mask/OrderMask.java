package org.enso.table.data.mask;

import java.util.Arrays;
import java.util.List;

/** Describes a storage reordering operator. */
public class OrderMask {
  private final int[] positions;

  /**
   * Creates a new reordering operator, with the specified characteristics. See {@link
   * #getPositions()} for a description of the semantics.
   *
   * @param positions the positions array, as described by {@link #getPositions()}
   */
  public OrderMask(int[] positions) {
    this.positions = positions;
  }

  /**
   * Describes the reordering that should happen on the applying storage.
   *
   * <p>The resulting storage should contain the {@code positions[i]}-th element of the original
   * storage at the i-th position. {@code positions[i]} may be equal to {@link
   * org.enso.table.data.index.Index.NOT_FOUND}, in which case a missing value should be inserted at
   * this position.
   */
  public int[] getPositions() {
    return positions;
  }

  public OrderMask append(OrderMask other) {
    int[] result = Arrays.copyOf(positions, positions.length + other.positions.length);
    System.arraycopy(other.positions, 0, result, positions.length, other.positions.length);
    return new OrderMask(result);
  }

  public static OrderMask empty() {
    return new OrderMask(new int[0]);
  }

  public static OrderMask fromList(List<Integer> positions) {
    int[] result = new int[positions.size()];
    for (int i = 0; i < positions.size(); i++) {
      result[i] = positions.get(i);
    }
    return new OrderMask(result);
  }
}
