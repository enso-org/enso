package org.enso.table.data.column.builder;

import org.enso.table.data.column.Column;
import org.enso.table.data.column.StringColumn;

import java.util.BitSet;

public class StringColumnBuilder extends ColumnBuilder {

  private String[] data;
  private int size;
  private BitSet isMissing;

  public StringColumnBuilder(String[] data, int size, BitSet isMissing) {
    this.data = data;
    this.size = size;
    this.isMissing = isMissing;
  }

  public StringColumnBuilder() {
    data = new String[64];
    size = 0;
    isMissing = new BitSet();
  }

  @Override
  public ColumnBuilder parseAndAppend(String value) {
    ensureAppendable();
    if (value == null) {
      isMissing.set(size++);
    } else {
      data[size++] = value;
    }
    return this;
  }

  private void ensureAppendable() {
    if (size >= data.length) {
      String[] newData = new String[2 * data.length];
      System.arraycopy(data, 0, newData, 0, data.length);
      data = newData;
    }
  }

  @Override
  public Column seal() {
    return new StringColumn(data, isMissing, size);
  }
}
