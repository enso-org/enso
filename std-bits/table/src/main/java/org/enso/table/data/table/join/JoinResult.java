package org.enso.table.data.table.join;

import org.enso.table.data.mask.OrderMask;

import java.util.ArrayList;
import java.util.List;

public class JoinResult {
  private final List<RowPair> matchedIndices;

  public JoinResult(List<RowPair> matchedIndices) {
    this.matchedIndices = matchedIndices;
  }

  //** Represents a pair of indices of matched rows. -1 means an unmatched row.*/
  public record RowPair(int leftIndex, int rightIndex) {}

  public OrderMask getLeftOrderMask() {
    return OrderMask.fromObjectList(matchedIndices, RowPair::leftIndex);
  }

  public OrderMask getRightOrderMask() {
    return OrderMask.fromObjectList(matchedIndices, RowPair::rightIndex);
  }

  public record BuilderSettings(
      boolean wantsCommon, boolean wantsLeftUnmatched, boolean wantsRightUnmatched) {}

  public static class Builder {
    List<RowPair> matchedIndices;

    final BuilderSettings settings;

    public Builder(int initialCapacity, BuilderSettings settings) {
      matchedIndices = new ArrayList<>(initialCapacity);
      this.settings = settings;
    }

    public Builder(BuilderSettings settings) {
      this(128, settings);
    }

    public void addMatchedRowsPair(int leftIndex, int rightIndex) {
      matchedIndices.add(new RowPair(leftIndex, rightIndex));
    }

    public void addUnmatchedLeftRow(int leftIndex) {
      matchedIndices.add(new RowPair(leftIndex, -1));
    }

    public void addUnmatchedRightRow(int rightIndex) {
      matchedIndices.add(new RowPair(-1, rightIndex));
    }

    public JoinResult build() {
      var tmp = matchedIndices;
      // Invalidate the builder to prevent further use.
      matchedIndices = null;
      return new JoinResult(tmp);
    }
  }
}
