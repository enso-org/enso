package org.enso.base;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;

public final class Stream_Utils {
  private Stream_Utils() {}

  /**
   * No conversion conversion. When the argument is already {@link InputStream} there is no need for
   * doing any further conversions.
   *
   * @param is
   * @return the {@code is} itself
   */
  public static InputStream asInputStream(InputStream is) {
    return is;
  }

  /**
   * No conversion conversion. When the argument is already {@link OutputStream} there is no need
   * for doing any further conversions.
   *
   * @param os
   * @return the {@code is} itself
   */
  public static OutputStream asOutputStream(OutputStream os) {
    return os;
  }


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

  /**
   * Copies the contents of the input sream to the output stream. If the number of bytes copied is
   * greater than maxLength, abort the cpoy and return false; otherwise return true.
   */
  public static boolean limitedCopy(
      InputStream inputStream, OutputStream outputStream, long maxLength) throws IOException {
    byte buffer[] = new byte[4096];
    long numBytesRead = 0;
    while (true) {
      int n = inputStream.read(buffer);
      if (n <= 0) {
        break;
      }
      if (numBytesRead + n <= maxLength) {
        outputStream.write(buffer, 0, n);
      }
      numBytesRead += n;
      if (numBytesRead > maxLength) {
        return false;
      }
    }
    return true;
  }
}
