package org.enso.table.data.index;

import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.enso.table.data.table.Column;

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
  public Column toColumn() {
    return null;
  }

  @Override
  public Index mask(BitSet mask, int cardinality) {
    return new DefaultIndex(cardinality);
  }

  @Override
  public Index countMask(int[] counts, int total) {
    return new DefaultIndex(total);
  }

  @Override
  public Index unique() {
    return this;
  }

  @Override
  public int size() {
    return size;
  }

  @Override
  public Index applyMask(OrderMask mask) {
    return this;
  }

  @Override
  public DefaultIndex slice(int offset, int limit) {
    return new DefaultIndex(Math.min(size, limit));
  }

  @Override
  public DefaultIndex slice(List<SliceRange> ranges) {
    return new DefaultIndex(SliceRange.totalLength(ranges));
  }
}
