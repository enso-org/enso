package org.enso.interpreter.arrow;

import org.enso.interpreter.arrow.runtime.SizeInBytes;

public enum LogicalLayout implements SizeInBytes {
  Date32(32),
  Date64(64),
  Int8(8),
  Int16(16),
  Int32(32),
  Int64(64);

  private final int bits;

  LogicalLayout(int bits) {
    this.bits = bits;
  }

  @Override
  public int sizeInBytes() {
    return bits / 8;
  }
}
