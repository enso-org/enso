package org.enso.table.data.table.join;

import org.enso.base.arrays.IntArrayBuilder;
import org.enso.table.data.mask.OrderMask;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

public record JoinResult(int[] matchedRowsLeftIndices, int[] matchedRowsRightIndices) {

  public OrderMask getLeftOrderMask() {
    return new OrderMask(matchedRowsLeftIndices);
  }

  public OrderMask getRightOrderMask() {
    return new OrderMask(matchedRowsRightIndices);
  }

  public Set<Integer> leftMatchedRows() {
    return new HashSet<>(Arrays.stream(matchedRowsLeftIndices).boxed().collect(Collectors.toList()));
  }

  public Set<Integer> rightMatchedRows() {
    return new HashSet<>(Arrays.stream(matchedRowsRightIndices).boxed().collect(Collectors.toList()));
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

    public void addRow(int leftIndex, int rightIndex) {
      leftIndices.add(leftIndex);
      rightIndices.add(rightIndex);
    }

    public JoinResult build() {
      return new JoinResult(leftIndices.build(), rightIndices.build());
    }
  }
}
