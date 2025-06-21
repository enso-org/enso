package org.enso.table.data.table.join;

import org.enso.base.arrays.LongArrayList;
import org.enso.table.data.mask.OrderMask;

public class JoinResult {
  private final long[] leftIndices;
  private final long[] rightIndices;

  public JoinResult(long[] leftIndices, long[] rightIndices) {
    this.leftIndices = leftIndices;
    this.rightIndices = rightIndices;
  }

  public OrderMask getLeftOrderMask() {
    return OrderMask.fromArray(leftIndices);
  }

  public OrderMask getRightOrderMask() {
    return OrderMask.fromArray(rightIndices);
  }

  public static class Builder {
    LongArrayList leftIndices;
    LongArrayList rightIndices;

    public Builder(int initialCapacity) {
      leftIndices = new LongArrayList(initialCapacity);
      rightIndices = new LongArrayList(initialCapacity);
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
      return new JoinResult(left.toArray(), right.toArray());
    }
  }
}
