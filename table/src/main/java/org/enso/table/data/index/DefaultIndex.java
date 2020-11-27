package org.enso.table.data.index;

import java.util.BitSet;
import java.util.Collections;
import java.util.List;

public class DefaultIndex extends Index {
  private final int size;

  public DefaultIndex(int size) {
    this.size = size;
  }

  @Override
  public Integer iloc(int loc) {
    return loc;
  }

  @Override
  public List<Integer> loc(Object item) {
    if (item instanceof Integer) {
      if ((Integer) item < size) {
        return Collections.singletonList((Integer) item);
      }
    } else if (item instanceof Long) {
      long l = (Long) item;
      if (l < size) {
        return Collections.singletonList((int) l);
      }
    }
    return null;
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
    return new DefaultIndex(cardinality);
  }

  @Override
  public Index countMask(int[] counts, int total) {
    return new DefaultIndex(total);
  }
}
