package org.enso.table.data.table.join;

import org.enso.base.arrays.IntArrayBuilder;
import org.enso.table.data.mask.OrderMask;

public class JoinResult {
  private final int length;
  private final int[] leftIndices;
  private final int[] rightIndices;

  public JoinResult(int[] leftIndices, int[] rightIndices, int length) {
    this.length = length;
    this.leftIndices = leftIndices;
    this.rightIndices = rightIndices;
  }

  // ** Represents a pair of indices of matched rows. -1 means an unmatched row.*/
  public record RowPair(int leftIndex, int rightIndex) {}

  public OrderMask getLeftOrderMask() {
    return OrderMask.fromArray(leftIndices, length);
  }

  public OrderMask getRightOrderMask() {
    return OrderMask.fromArray(rightIndices, length);
  }

  public static class Builder {
    IntArrayBuilder leftIndices;
    IntArrayBuilder rightIndices;

    public Builder(int initialCapacity) {
      leftIndices = new IntArrayBuilder(initialCapacity);
      rightIndices = new IntArrayBuilder(initialCapacity);
    }

    public Builder() {
      this(128);
    }

    public void addMatchedRowsPair(int leftIndex, int rightIndex) {
      leftIndices.add(leftIndex);
      rightIndices.add(rightIndex);
    }

    public void addUnmatchedLeftRow(int leftIndex) {
      leftIndices.add(leftIndex);
      rightIndices.add(-1);
    }

    public void addUnmatchedRightRow(int rightIndex) {
      leftIndices.add(-1);
      rightIndices.add(rightIndex);
    }

    /**
     * Returns the result of the builder.
     *
     * <p>This method avoids copying for performance. After calling this method, the builder is
     * invalidated and cannot be used anymore. Any usage of the builder afterwards will result in a
     * {@code NullPointerException}.
     */
    public JoinResult buildAndInvalidate() {
      var left = leftIndices;
      var right = rightIndices;
      leftIndices = null;
      rightIndices = null;
      return new JoinResult(
          left.unsafeGetResultAndInvalidate(),
          right.unsafeGetResultAndInvalidate(),
          left.getLength());
    }
  }
}
