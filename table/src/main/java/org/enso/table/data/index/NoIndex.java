package org.enso.table.data.index;

import java.util.BitSet;

public class NoIndex extends Index {

  @Override
  public Object iloc(int loc) {
    return loc;
  }

  @Override
  public int loc(Object loc) {
    return loc instanceof Long ? (int) loc : NOT_FOUND;
  }

  @Override
  public String ilocString(int loc) {
    return String.valueOf(loc);
  }

  @Override
  public String getName() {
    return "";
  }

  @Override
  public Index mask(BitSet mask, int cardinality) {
    return this;
  }
}
