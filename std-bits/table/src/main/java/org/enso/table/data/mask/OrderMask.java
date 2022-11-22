package org.enso.table.data.mask;

/** Describes a storage reordering operator. */
public class OrderMask {
  public static OrderMask repeatRows(int rowCount, int repeats) {
    int[] result = new int[rowCount * repeats];
    for (int row = 0; row < rowCount; row++) {
      for (int repeat = 0; repeat < repeats; repeat++) {
        result[row * repeats + repeat] = row;
      }
    }
    return new OrderMask(result);
  }

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
}
