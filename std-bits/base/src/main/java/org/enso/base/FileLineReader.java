package org.enso.base;

import com.ibm.icu.text.Normalizer2;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.enso.base.arrays.LongArrayList;
import org.enso.polyglot.common_utils.Core_Text_Utils;
import org.graalvm.polyglot.Context;

public class FileLineReader {
  public static class ByteArrayOutputStreamWithContains extends ByteArrayOutputStream {
    public ByteArrayOutputStreamWithContains(int size) {
      super(size);
    }

    // ** Creates a preloaded stream from a byte array. */
    public static ByteArrayOutputStreamWithContains fromByteArray(byte[] bytes) {
      var stream = new ByteArrayOutputStreamWithContains(0);
      stream.buf = bytes;
      stream.count = bytes.length;
      return stream;
    }

    public boolean contains(byte[] bytes) {
      if (bytes.length > count) {
        return false;
      }
      for (int i = 0; i < count - bytes.length; i++) {
        boolean found = true;
        for (int j = 0; j < bytes.length; j++) {
          if (buf[i + j] != bytes[j]) {
            found = false;
            break;
          }
        }
        if (found) {
          return true;
        }
      }
      return false;
    }
  }

  private static class CancellationToken {
    public boolean isCancelled = false;

    public void cancel() {
      isCancelled = true;
    }
  }

  private static final Logger LOGGER = Logger.getLogger("enso-file-line-reader");

  // ** Amount of data to read at a time for a single line (4KB). */
  private static final int LINE_BUFFER = 4 * 1024;

  // ** Amount of data to read at a time (4MB). */
  private static final int BUFFER_SIZE = 4 * 1024 * 1024;

  private static boolean moreToRead(int c, MappedByteBuffer buffer) {
    return switch (c) {
      case '\n', -1 -> false;
      case '\r' -> {
        c = buffer.hasRemaining() ? buffer.get() : '\n';
        if (c != '\n') {
          buffer.position(buffer.position() - 1);
        }
        yield false;
      }
      default -> true;
    };
  }

  private static int readByte(MappedByteBuffer buffer) {
    return buffer.hasRemaining() ? buffer.get() : -1;
  }

  // ** Reads a line into an OutputStream.
  //    Returns true if the end of the line was found, false if the buffer finished. */
  private static boolean readLine(MappedByteBuffer buffer, ByteArrayOutputStream result) {
    int c = readByte(buffer);
    while (moreToRead(c, buffer)) {
      result.write(c);
      c = readByte(buffer);
      Context.getCurrent().safepoint();
    }
    return c != -1 && (c != '\r' || buffer.hasRemaining());
  }

  // ** Scans forward one line.
  //    Returns true if the end of the line was found, false if the buffer finished. */
  private static boolean scanLine(MappedByteBuffer buffer) {
    int c = readByte(buffer);
    while (moreToRead(c, buffer)) {
      c = readByte(buffer);
      Context.getCurrent().safepoint();
    }
    return c != -1 && (c != '\r' || buffer.hasRemaining());
  }

  // ** Reads a line from a file at the given index using the existing rowMap. */
  private static String readLineByIndex(File file, LongArrayList rowMap, int index, Charset charset)
      throws IOException {
    if (index >= rowMap.getSize()) {
      throw new IndexOutOfBoundsException(index);
    }

    long length = file.length();
    long position = rowMap.get(index);
    if (position >= length) {
      return null;
    }
    long toRead =
        rowMap.getSize() > index + 1 ? rowMap.get(index + 1) - position : length - position;

    // Output buffer
    var outputStream = new ByteArrayOutputStream(128);

    // Only read what we have to.
    try (var stream = new FileInputStream(file)) {
      var channel = stream.getChannel();
      int bufferSize = (int) Math.min(LINE_BUFFER, toRead);
      long remaining = toRead - bufferSize;
      var buffer = channel.map(FileChannel.MapMode.READ_ONLY, position, bufferSize);
      var result = readLine(buffer, outputStream);
      while (!result && remaining > 0) {
        position += bufferSize;
        bufferSize = (int) Math.min(LINE_BUFFER, remaining);
        remaining -= bufferSize;
        buffer = channel.map(FileChannel.MapMode.READ_ONLY, position, bufferSize);
        result = readLine(buffer, outputStream);
      }
    }

    return outputStream.toString(charset);
  }

  // ** Scans forward in a file and returns the line at the given index. */
  public static String readSingleLine(
      File file,
      LongArrayList rowMap,
      int index,
      Charset charset,
      Function<ByteArrayOutputStreamWithContains, String> filter)
      throws IOException {
    int size = rowMap.getSize();
    if (index != -1 && size > index) {
      return readLineByIndex(file, rowMap, index, charset);
    }

    // Start at the last known line and scan forward.
    return forEachLine(file, rowMap, size - 1, index, charset, filter, null);
  }

  public static List<String> readLines(
      File file,
      LongArrayList rowMap,
      int startAt,
      int endAt,
      Charset charset,
      Function<ByteArrayOutputStreamWithContains, String> filter)
      throws IOException {
    List<String> result = new ArrayList<>();
    forEachLine(file, rowMap, startAt, endAt, charset, filter, (index, line) -> result.add(line));
    return result;
  }

  // ** Scans forward in a file reading line by line.
  // * @param file The file to read.
  // * @param rowMap The rowMap to use.
  // * @param startAt The index to start at.
  // * @param endAt The index to end at (inclusive).
  // * @param charset The charset to use.
  // * @param filter The filter to apply to each line.
  // * @param action The action to apply to each line (optional).
  // * @return The last line read or null if end of file is reached.
  // * *//
  public static String forEachLine(
      File file,
      LongArrayList rowMap,
      int startAt,
      int endAt,
      Charset charset,
      Function<ByteArrayOutputStreamWithContains, String> filter,
      BiConsumer<Integer, String> action)
      throws IOException {
    return innerForEachLine(
        file, rowMap, startAt, endAt, charset, filter, action, new CancellationToken());
  }

  private static String innerForEachLine(
      File file,
      LongArrayList rowMap,
      int startAt,
      int endAt,
      Charset charset,
      Function<ByteArrayOutputStreamWithContains, String> filter,
      BiConsumer<Integer, String> action,
      CancellationToken cancellationToken)
      throws IOException {
    if (startAt >= rowMap.getSize()) {
      throw new IndexOutOfBoundsException(startAt);
    }
    int index = action == null ? rowMap.getSize() - 1 : startAt;

    long length = file.length();
    long position = rowMap.get(index);
    if (position >= length) {
      return null;
    }

    boolean readAll = filter != null || action != null || endAt == -1;
    var outputStream = new ByteArrayOutputStreamWithContains(128);
    String output = null;

    try (var stream = new FileInputStream(file)) {
      var channel = stream.getChannel();

      var bufferSize = (int) Math.min(BUFFER_SIZE, (length - position));
      var truncated = bufferSize != (length - position);
      var buffer = channel.map(FileChannel.MapMode.READ_ONLY, position, bufferSize);

      // Loop until we either reach the required record or run out of data.
      while (!cancellationToken.isCancelled
          && (endAt == -1 || index <= endAt)
          && (truncated || buffer.hasRemaining())) {
        var linePosition = buffer.position() + position;

        // Read a line.
        outputStream.reset();
        boolean success =
            (readAll || index == endAt) ? readLine(buffer, outputStream) : scanLine(buffer);

        if (success || !truncated) {
          String line = null;
          if (filter == null || (line = filter.apply(outputStream)) != null) {
            if (index >= rowMap.getSize()) {
              rowMap.add(linePosition);
            }

            if (action != null) {
              line = line == null ? outputStream.toString(charset) : line;
              action.accept(index, line);
            }

            if (index == endAt) {
              output = line == null ? outputStream.toString(charset) : line;
            }

            if (index % 100000 == 0) {
              LOGGER.log(Level.INFO, "Scanned Lines: {0}", index);
            }
            index++;

            // If no filter we can record the start of the next line.
            if (filter == null && index == rowMap.getSize()) {
              rowMap.add(buffer.position() + position);
            }
          }

          // Fast-forward if needed
          if (filter != null && index < rowMap.getSize()) {
            int newPosition = Math.min(bufferSize, (int) (rowMap.get(index) - position));
            buffer.position(newPosition);
          }
        } else {
          // Read more if we need to
          if (!buffer.hasRemaining()) {
            position = linePosition;
            bufferSize = (int) Math.min(BUFFER_SIZE, (length - position));
            truncated = bufferSize != (length - position);
            buffer = channel.map(FileChannel.MapMode.READ_ONLY, position, bufferSize);
          }
        }
      }

      if (!truncated && !buffer.hasRemaining() && rowMap.get(rowMap.getSize() - 1) != length) {
        // Add the last line to mark reached the end.
        rowMap.add(length);
      }

      return output;
    }
  }

  // ** Scans forward in a file reading line by line until it finds a line that matches the new
  // filter. */
  public static long findFirstNewFilter(
      File file,
      LongArrayList rowMap,
      int endAt,
      Charset charset,
      Function<ByteArrayOutputStreamWithContains, String> filter,
      Function<ByteArrayOutputStreamWithContains, String> newFilter)
      throws IOException {
    final CancellationToken token = new CancellationToken();
    final List<Long> result = new ArrayList<>();
    BiConsumer<Integer, String> action =
        (index, line) -> {
          var bytes = line.getBytes(charset);
          var outputStream = ByteArrayOutputStreamWithContains.fromByteArray(bytes);
          if (newFilter.apply(outputStream) != null) {
            result.add(rowMap.get(index));
            token.cancel();
          }
        };
    innerForEachLine(file, rowMap, 0, endAt, charset, filter, action, token);
    return result.isEmpty() ? rowMap.get(rowMap.getSize() - 1) : result.get(0);
  }

  // ** Creates a filter that checks if the line contains the given string. */
  public static Function<ByteArrayOutputStreamWithContains, String> createContainsFilter(
      String contains, Charset charset) {
    if (isUnicodeCharset(charset)) {
      var nfcVersion = Normalizer2.getNFCInstance().normalize(contains);
      var nfdVersion = Normalizer2.getNFDInstance().normalize(contains);
      if (!nfcVersion.equals(nfdVersion)) {
        // Need to use Unicode normalization for equality.
        return (outputStream) -> {
          var line = outputStream.toString(charset);
          return Core_Text_Utils.compare_normalized(contains, line) == 0 ? line : null;
        };
      }
    }

    var bytes = contains.getBytes(charset);
    return (outputStream) -> outputStream.contains(bytes) ? outputStream.toString(charset) : null;
  }

  // ** Wraps an Enso function filter in a FileLineReader filter. */
  public static Function<ByteArrayOutputStreamWithContains, String> wrapBooleanFilter(
      Function<String, Boolean> filter, Charset charset) {
    return (outputStream) -> {
      var line = outputStream.toString(charset);
      return filter.apply(line) ? line : null;
    };
  }

  // Joins two filters together. */
  public static Function<ByteArrayOutputStreamWithContains, String> mergeTwoFilters(
      Function<ByteArrayOutputStreamWithContains, String> first,
      Function<ByteArrayOutputStreamWithContains, String> second) {
    return (outputStream) -> {
      var first_result = first.apply(outputStream);
      return first_result != null ? second.apply(outputStream) : null;
    };
  }

  private static boolean isUnicodeCharset(Charset charset) {
    return charset == StandardCharsets.UTF_8
        || charset == StandardCharsets.UTF_16
        || charset == StandardCharsets.UTF_16BE
        || charset == StandardCharsets.UTF_16LE;
  }
}
