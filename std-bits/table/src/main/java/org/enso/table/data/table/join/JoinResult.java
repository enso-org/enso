package org.enso.table.data.table.join;

import org.enso.base.arrays.IntArrayBuilder;
import org.enso.table.data.mask.OrderMask;

public record JoinResult(int[] matchedRowsLeftIndices, int[] matchedRowsRightIndices) {

  public OrderMask getLeftOrderMask() {
    return new OrderMask(matchedRowsLeftIndices);
  }

  public OrderMask getRightOrderMask() {
    return new OrderMask(matchedRowsRightIndices);
  }

  public record BuilderSettings(
      boolean wantsCommon, boolean wantsLeftUnmatched, boolean wantsRightUnmatched) {}

  public static class Builder {
    IntArrayBuilder leftIndices;
    IntArrayBuilder rightIndices;

    final BuilderSettings settings;

    public Builder(int initialCapacity, BuilderSettings settings) {
      leftIndices = new IntArrayBuilder(initialCapacity);
      rightIndices = new IntArrayBuilder(initialCapacity);
      this.settings = settings;
    }

    public Builder(BuilderSettings settings) {
      this(128, settings);
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

    public JoinResult build() {
      return new JoinResult(leftIndices.build(), rightIndices.build());
    }
  }
}
