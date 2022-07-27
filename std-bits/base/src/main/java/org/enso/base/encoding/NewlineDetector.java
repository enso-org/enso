package org.enso.base.encoding;

import java.io.IOException;
import java.io.Reader;

/** A reader that wraps another reader and checks if a newline character has been encountered. */
public class NewlineDetector extends Reader {
  private final Reader underlying;
  private boolean newlineEncountered = false;

  public NewlineDetector(Reader underlying) {
    this.underlying = underlying;
  }

  @Override
  public int read(char[] cbuf, int off, int len) throws IOException {
    int read = underlying.read(cbuf, off, len);

    if (!newlineEncountered) {
      for (int i = 0; i < read; ++i) {
        char c = cbuf[off + i];
        if (c == '\n' || c == '\r') {
          newlineEncountered = true;
          break;
        }
      }
    }

    return read;
  }

  @Override
  public void close() throws IOException {
    underlying.close();
  }

  /** Checks if a newline character has been encountered within data that has been read so far. */
  public boolean newlineEncountered() {
    return newlineEncountered;
  }
}
