package org.enso.base;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

public final class Stream_Utils {
  private Stream_Utils() {}

  /**
   * Conversion interface. Any Enso/Truffle object with invocable {@read} member that takes three
   * arguments is eligible for being treated as host Java {@link InputStream}. There are two
   * overloaded {@link #asInputStream conversion methods}. The <em>hosted Java interop</em> system
   * of Truffle will pick the more suitable one depending on the type of argument.
   *
   * @see #asInputStream
   */
  public static interface InputStreamLike {
    public int read(byte[] arr, int off, int len) throws IOException;
  }

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
   * Conversion to {@link InputStream}. When the argument <em>looks like</em> an input stream, let's
   * wrap it.
   *
   * @param inputStreamLike any guest object with {@code read} method
   * @return proper
   */
  public static InputStream asInputStream(InputStreamLike inputStreamLike) {
    return new GuestInputStream(inputStreamLike);
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

  private static class GuestInputStream extends InputStream {
    private final InputStreamLike inputStreamLike;

    private GuestInputStream(InputStreamLike inputStreamLike) {
      this.inputStreamLike = inputStreamLike;
    }

    @Override
    public int read(byte[] b, int off, int len) throws IOException {
      return inputStreamLike.read(b, off, len);
    }

    @Override
    public int read() throws IOException {
      byte[] arr = new byte[1];
      int read = read(arr, 0, 1);
      if (read == -1) {
        return -1;
      }
      if (read != 1) {
        throw new IOException();
      }
      return arr[0];
    }

    @Override
    public int read(byte[] b) throws IOException {
      return read(b, 0, b.length);
    }
  }
}
