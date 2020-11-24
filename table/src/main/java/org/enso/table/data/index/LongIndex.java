package org.enso.table.data.index;

import org.enso.table.data.column.storage.LongStorage;

import java.util.BitSet;

public class LongIndex extends Index {
  public static LongIndex fromStorage(LongStorage storage) {
    System.out.println("MIN: " + storage.min());
    System.out.println("MAX: " + storage.max());
    System.out.println("SIZE: " + storage.size());
    return null;
  }

  @Override
  public Object iloc(int loc) {
    return null;
  }

  @Override
  public String ilocString(int loc) {
    return null;
  }

  @Override
  public int loc(Object loc) {
    return 0;
  }

  @Override
  public String getName() {
    return null;
  }

  @Override
  public Index mask(BitSet mask, int cardinality) {
    return null;
  }
}
