package org.enso.table.data.table.join;

import static org.enso.table.data.column.builder.Builder.checkSize;

import org.enso.base.arrays.LongArrayList;
import org.enso.table.data.column.operation.masks.IndexMapper;

public class JoinResult {
  private final IndexMapper leftIndexMapper;
  private final IndexMapper rightIndexMapper;

  public JoinResult(long[] leftIndices, long[] rightIndices) {
    this.leftIndexMapper = new IndexMapper.ArrayMapping(leftIndices);
    this.rightIndexMapper = new IndexMapper.ArrayMapping(rightIndices);
  }

  public IndexMapper getLeftIndexMapper() {
    return leftIndexMapper;
  }

  public IndexMapper getRightIndexMapper() {
    return rightIndexMapper;
  }

  public static class Builder {
    LongArrayList leftIndices;
    LongArrayList rightIndices;

    public Builder(long initialCapacity) {
      int capacity = checkSize(initialCapacity);
      leftIndices = new LongArrayList(capacity);
      rightIndices = new LongArrayList(capacity);
    }

    public Builder() {
      this(128);
    }

    public void addMatchedRowsPair(long leftIndex, long rightIndex) {
      leftIndices.add(leftIndex);
      rightIndices.add(rightIndex);
    }

    public void addUnmatchedLeftRow(long leftIndex) {
      leftIndices.add(leftIndex);
      rightIndices.add(IndexMapper.NOT_FOUND_INDEX);
    }

    public void addUnmatchedRightRow(long rightIndex) {
      leftIndices.add(IndexMapper.NOT_FOUND_INDEX);
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
