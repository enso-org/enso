package org.enso.base_test_helpers;

import java.io.IOException;
import java.io.InputStream;

/** A generic stream used for tests. */
public class RangeStream extends InputStream {
  private final int end;
  private int current;

  public RangeStream(int start, int end) {
    assert 0 <= start && start <= end;
    this.end = end;
    this.current = start;
  }

  @Override
  public int read() throws IOException {
    if (current >= end) {
      return -1;
    } else {
      return current++ % 256;
    }
  }
}
