package org.enso.table.data.column;

import java.util.BitSet;

public class StringColumn extends Column {
  private final String[] data;
  private final BitSet isMissing;
  private final int size;

  public StringColumn(String[] data, BitSet isMissing, int size) {
    this.data = data;
    this.isMissing = isMissing;
    this.size = size;
  }

  @Override
  public long size() {
    return size;
  }

  public String getItem(long idx) {
    return data[(int) idx];
  }

  @Override
  public long getType() {
    return Type.STRING;
  }

  @Override
  public boolean isNa(long idx) {
    return idx >= size || isMissing.get((int) idx);
  }
}
