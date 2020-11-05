package org.enso.table.data.column;

import org.enso.table.data.column.builder.PrimInferredColumnBuilder;

import java.util.BitSet;

public class LongColumn extends Column {
  private final long[] data;
  private final BitSet isMissing;
  private final int size;

  public LongColumn(long[] data, int size, BitSet isMissing) {
    this.data = data;
    this.isMissing = isMissing;
    this.size = size;
  }

  @Override
  public long size() {
    return size;
  }

  public long getItem(long idx) {
    return data[(int) idx];
  }

  @Override
  public long getType() {
    return Type.LONG;
  }

  @Override
  public boolean isNa(long idx) {
    return isMissing.get((int) idx);
  }
}
