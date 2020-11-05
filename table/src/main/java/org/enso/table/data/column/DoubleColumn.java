package org.enso.table.data.column;

import org.enso.table.data.column.builder.PrimInferredColumnBuilder;

import java.util.BitSet;

public class DoubleColumn extends Column {
  private final long[] data;
  private final BitSet isMissing;
  private final int size;

  public DoubleColumn(long[] data, int size, BitSet isMissing) {
    this.data = data;
    this.isMissing = isMissing;
    this.size = size;
  }

  @Override
  public long size() {
    return size;
  }

  public double getItem(long idx) {
    return Double.longBitsToDouble(data[(int) idx]);
  }

  @Override
  public long getType() {
    return Type.DOUBLE;
  }

  @Override
  public boolean isNa(long idx) {
    return isMissing.get((int) idx);
  }
}
