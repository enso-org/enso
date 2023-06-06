package org.enso.table.data.column.storage.numeric;

public class LongConstantStorage extends ComputedLongStorage {
  private final long constant;

  public LongConstantStorage(long constant, int size) {
    super(size);
    this.constant = constant;
  }

  @Override
  protected long computeItem(int idx) {
    return constant;
  }
}
