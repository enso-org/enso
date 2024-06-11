package org.enso.base;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

public class Stream_Utils {
  public static byte[] peek(InputStream stream, int n) throws IOException {
    assert n >= 0;
    assert stream.markSupported();

    byte[] buffer = new byte[n];
    stream.mark(n + 1);
    int offset = 0;
    while (offset < n) {
      int read = stream.read(buffer, offset, n - offset);
      if (read == -1) {
        break;
      }
      offset += read;
    }
    stream.reset();
    if (offset < n) {
      buffer = Arrays.copyOf(buffer, offset);
    }
    return buffer;
  }
}
