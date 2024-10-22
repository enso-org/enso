package org.enso.base;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
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

    public default int available() {
      return 0;
    }
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

  /**
   * Conversion interface. Any Enso/Truffle object with invocable {@write} member that takes three
   * arguments is eligible for being treated as host Java {@link OutputStream}. There are two
   * overloaded {@link #asOutputStream conversion methods}. The <em>hosted Java interop</em> system
   * of Truffle will pick the more suitable one depending on the type of argument.
   *
   * @see #asOutputStream
   */
  public static interface OutputStreamLike {
    public void write(byte[] arr, int off, int len) throws IOException;
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

  /**
   * Conversion to {@link OutputStream}. When the argument <em>looks like</em> an output stream,
   * let's wrap it.
   *
   * @param outputStreamLike any guest object with {@code write} method
   * @return proper
   */
  public static OutputStream asOutputStream(OutputStreamLike outputStreamLike) {
    return new GuestOutputStream(outputStreamLike);
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

    @Override
    public int available() throws IOException {
      try {
        return inputStreamLike.available();
      } catch (Error | Exception e) {
        return 0;
      }
    }
  }

  private static final class GuestOutputStream extends OutputStream {

    private final OutputStreamLike outputStreamLike;

    private GuestOutputStream(OutputStreamLike os) {
      this.outputStreamLike = os;
    }

    @Override
    public void write(int b) throws IOException {
      byte[] arr = new byte[] {(byte) b};
      write(arr, 0, 1);
    }

    @Override
    public void write(byte[] b, int off, int len) throws IOException {
      outputStreamLike.write(b, off, len);
    }
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
