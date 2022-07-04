package org.enso.table.data.column.storage;

import org.enso.table.data.mask.OrderMask;

import java.util.BitSet;

public class NullStorage extends Storage {
  private final int size;

  public NullStorage(int size) {
    this.size = size;
  }

  @Override
  public int size() {
    return this.size;
  }

  @Override
  public int countMissing() {
    return this.size;
  }

  @Override
  public long getType() { return Type.OBJECT; }

  @Override
  public boolean isNa(long idx) {
    return true;
  }

  @Override
  public Object getItemBoxed(int idx) {
    return null;
  }

  @Override
  protected boolean isOpVectorized(String name) {
    return false;
  }

  @Override
  protected Storage runVectorizedMap(String name, Object argument) {
    return null;
  }

  @Override
  protected Storage runVectorizedZip(String name, Storage argument) {
    return null;
  }

  @Override
  public Storage mask(BitSet mask, int cardinality) { return new NullStorage(cardinality); }

  @Override
  public Storage applyMask(OrderMask mask) { return new NullStorage(mask.getPositions().length); }

  @Override
  public Storage countMask(int[] counts, int total) {
    return null;
  }

  @Override
  public Storage slice(int offset, int limit) {
    return new NullStorage(Math.min(size - offset, limit));
  }
}
