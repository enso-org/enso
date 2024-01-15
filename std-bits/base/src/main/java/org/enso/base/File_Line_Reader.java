package org.enso.base;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.Charset;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.enso.base.arrays.LongArrayList;
import org.graalvm.polyglot.Context;

public class File_Line_Reader {
  private static final ByteArrayOutputStream reusedLine = new ByteArrayOutputStream();
  private static final Logger LOGGER = Logger.getLogger(File_Line_Reader.class.getName());
  private static final int LINE_BUFFER = 4 * 1024;
  private static final int BUFFER_SIZE = 4 * 1024 * 1024;

  private static boolean moreToRead(int c, MappedByteBuffer buffer) {
    if (c == -1 || c == '\n') {
      return false;
    }

    if (c == '\r') {
      c = buffer.hasRemaining() ? buffer.get() : '\n';
      if (c != '\n') {
        buffer.position(buffer.position() - 1);
      }
      return false;
    }

    return true;
  }

  private static String readLine(
      MappedByteBuffer buffer, Charset charset, FileChannel channel, long position)
      throws IOException {
    reusedLine.reset();
    int c = buffer.get();
    while (moreToRead(c, buffer)) {
      reusedLine.write(c);

      if (buffer.hasRemaining()) {
        c = buffer.get();
      } else {
        // Need to read more data.
        if (channel != null) {
          position = position + buffer.position();
          buffer = channel.map(FileChannel.MapMode.READ_ONLY, position, LINE_BUFFER);
          c = buffer.get();
        } else {
          c = -1;
        }
      }

      Context.getCurrent().safepoint();
    }

    return reusedLine.toString(charset);
  }

  private static void scanLine(MappedByteBuffer buffer) {
    int c = buffer.get();
    while (moreToRead(c, buffer)) {
      c = buffer.hasRemaining() ? buffer.get() : -1;
      Context.getCurrent().safepoint();
    }
  }

  // ** Reads a line from a file at the given index using the existing rowMap. */
  public static String readLineByIndex(File file, LongArrayList rowMap, int index, Charset charset)
      throws IOException {
    if (index >= rowMap.getSize()) {
      throw new IndexOutOfBoundsException(index);
    }

    long length = file.length();
    long position = rowMap.get(index);
    if (position >= length) {
      return null;
    }

    // Only read what we have to.
    try (var stream = new FileInputStream(file)) {
      var channel = stream.getChannel();
      int bufferSize =
          rowMap.getSize() > index + 1
              ? (int) (rowMap.get(index + 1) - position)
              : (int)Math.min(LINE_BUFFER, (length - position));
      var buffer = channel.map(FileChannel.MapMode.READ_ONLY, position, bufferSize);
      return readLine(buffer, charset, channel, position);
    }
  }

  // ** Scans forward in a file and returns the line at the given index. */
  public static String scanAndReadLine(
      File file, LongArrayList rowMap, int index, Charset charset, Function<String, Boolean> filter)
      throws IOException {
    int size = rowMap.getSize();
    if (index != -1 && size > index) {
      return readLineByIndex(file, rowMap, index, charset);
    }

    // Start at the last known line and scan forward.
    return readLines(file, rowMap, size - 1, index, charset, filter, null);
  }

  // ** Scans forward in a file reading line by line.
  // * @param file The file to read.
  // * @param rowMap The rowMap to use.
  // * @param startAt The index to start at.
  // * @param endAt The index to end at (inclusive).
  // * @param charset The charset to use.
  // * @param filter The filter to apply to each line.
  // * @param action The action to apply to each line.
  // * @return The last line read or null if end of file is reached.
  // * *//
  public static String readLines(
      File file,
      LongArrayList rowMap,
      int startAt,
      int endAt,
      Charset charset,
      Function<String, Boolean> filter,
      BiFunction<Integer, String, Boolean> action)
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
    String output = null;

    try (var stream = new FileInputStream(file)) {
      var channel = stream.getChannel();

      var bufferSize =  (int)Math.min(BUFFER_SIZE, (length - position));
      var truncated = bufferSize != (length - position);
      var buffer = channel.map(FileChannel.MapMode.READ_ONLY, position, bufferSize);

      // If we have an action then apply it to the lines we already know about.
      if (action != null) {
        while (index < Math.min(endAt, rowMap.getSize() - 1)) {
          // Do we need to read more data?
          if (truncated && rowMap.get(index + 1) > position + buffer.position()) {
            position = rowMap.get(index);
            bufferSize = (int)Math.min(BUFFER_SIZE, (length - position));
            truncated = bufferSize != (length - position);
            buffer = channel.map(FileChannel.MapMode.READ_ONLY, position, bufferSize);
          } else if (filter != null) {
            buffer.position((int) (rowMap.get(index) - position));
          }

          output = readLine(buffer, charset, null, -1);
          if (action.apply(index, output)) {
            return output;
          }

          index++;
        }
      }

      // Scan forward until we reach the end or the endAt index.
      while (endAt == -1 || index <= endAt) {
        var linePosition = buffer.position() + position;
        if (linePosition >= length) {
          // Reached end of file.
          break;
        }

        if (readAll || index == endAt) {
          output = readLine(buffer, charset, null, -1);
        } else {
          scanLine(buffer);
        }

        if ((buffer.hasRemaining() || !truncated) && (filter == null || filter.apply(output))) {
          if (index >= rowMap.getSize()) {
            if (index % 100000 == 0) {
              LOGGER.log(Level.INFO, "Scanned Lines: {0}", index);
            }
            rowMap.add(linePosition);
          }

          index++;
          if (filter == null && index >= rowMap.getSize()) {
            rowMap.add(buffer.position() + position);
          }

          // Callback on action and stop if requested.
          if (action != null && !action.apply(index - 1, output)) {
            return output;
          }
        }

        if (!buffer.hasRemaining()) {
          if (!truncated) {
            // Reached end of file.
            break;
          }

          // Need to read more data.
          position = rowMap.get(index);
          bufferSize = (int) Math.min(BUFFER_SIZE, (length - position));
          truncated = bufferSize != (length - position);
          buffer = channel.map(FileChannel.MapMode.READ_ONLY, position, bufferSize);
        }
      }

      if (!truncated && !buffer.hasRemaining() && rowMap.get(rowMap.getSize() - 1) != length) {
        // Add the last line to mark reached the end.
        rowMap.add(length);
      }

      return output;
    }
  }
}
