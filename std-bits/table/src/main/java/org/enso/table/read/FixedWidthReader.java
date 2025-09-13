package org.enso.table.read;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.enso.base.encoding.ReportingStreamDecoder;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.parsing.DatatypeParser;
import org.enso.table.parsing.problems.CommonParseProblemAggregator;
import org.enso.table.parsing.problems.ParseProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public class FixedWidthReader {
  public static final int MAXIMUM_LINE_LENGTH = 1024 * 1024;

  private List<FixedWidthLayoutEntry> layoutEntries;
  private Justification justificationForLayoutInference;
  private final Charset charset;
  private final long skipRows;
  private final long rowLimit;
  private final InvalidFixedWidthRowsBehavior invalidRowsBehavior;

  // Null means Infer; otherwise, contains the exact line ending string.
  private String lineEnding;

  // Only used if lineEnding is the empty string; the layout must be specified
  // and cannot be inferred.
  private int lineLength = -1;

  private final boolean emptyToNull;
  private final DatatypeParser valueParser;
  private final FixedWidthDecodingProblemAggregator decodingProblemAggregator;
  private final FixedWidthReaderProblemAggregator problemAggregator;

  private List<BuilderForType<String>> builders = null;

  private int layoutWidth = 0;
  private boolean firstLine = true;
  private int firstLineLength = 0;
  private long sourceLineNumber = 0;
  private long tableRowNumber = 0;

  private static final int READ_BUFFER_INITIAL_SIZE = 128;
  private byte[] readBuffer = new byte[READ_BUFFER_INITIAL_SIZE];

  public FixedWidthReader(
      List<FixedWidthLayoutEntry> layoutEntries,
      Justification justificationForLayoutInference,
      Charset charset,
      long skipRows,
      long rowLimit,
      InvalidFixedWidthRowsBehavior invalidRowsBehavior,
      String lineEnding,
      boolean emptyToNull,
      DatatypeParser valueParser,
      boolean warningsAsErrors,
      FixedWidthDecodingProblemAggregator decodingProblemAggregator,
      ProblemAggregator problemAggregator) {

    assert layoutEntries == null ^ justificationForLayoutInference == null
        : "Exactly one of 'layoutEntries' and 'justificationForLayoutInference' can be specified";

    this.layoutEntries = layoutEntries;
    this.justificationForLayoutInference = justificationForLayoutInference;
    this.charset = charset;
    this.skipRows = skipRows;
    this.rowLimit = rowLimit;
    this.invalidRowsBehavior = invalidRowsBehavior;
    this.lineEnding = lineEnding;
    this.emptyToNull = emptyToNull;
    this.valueParser = valueParser;
    this.decodingProblemAggregator = decodingProblemAggregator;
    this.problemAggregator =
        new FixedWidthReaderProblemAggregator(
            problemAggregator, invalidRowsBehavior, warningsAsErrors);

    if (layoutEntries != null && layoutEntries.isEmpty()) {
      throw new IllegalArgumentException("Must specify at least one column");
    }

    if (layoutEntries == null && lineEnding != null && lineEnding.isEmpty()) {
      throw new IllegalArgumentException(
          "If the line ending is the empty string, the layout must be specified and cannot be"
              + " inferred.");
    }
  }

  public Table read(InputStream inputStream) throws IOException {
    var pushbackInputStream = allocatePushbackStream(inputStream);
    for (int i = 0; i < skipRows; ++i) {
      readLine(pushbackInputStream);
    }

    if (layoutEntries == null) {
      inferHeaders(pushbackInputStream);
    }

    layoutWidth = layoutEntries.get(layoutEntries.size() - 1).end();

    initBuilders(layoutEntries.size());
    byte[] readBuffer = new byte[layoutWidth];

    while (true) {
      int lineLength = readLine(pushbackInputStream);

      if (lineLength == -1 || (rowLimit != -1 && tableRowNumber >= rowLimit)) {
        break;
      }

      addRow(lineLength);

      sourceLineNumber++;
    }

    return makeFinalTable();
  }

  // lineLength is the length of the actual line from the input stream, which
  // might be larger than layoutWidth and the buffer capacity.
  private void addRow(int lineLength) throws IOException {
    Context context = Context.getCurrent();

    if (firstLine) {
      firstLine = false;
      firstLineLength = lineLength;
    } else {
      if (lineLength != firstLineLength) {
        problemAggregator.reportInconsistentLineLengths();
      }
    }

    if (lineLength < layoutWidth) {
      var trn = invalidRowsBehavior == InvalidFixedWidthRowsBehavior.KEEP ? tableRowNumber : null;
      problemAggregator.reportShortLine(sourceLineNumber, trn, lineLength, layoutWidth);
    }

    if (lineLength < layoutWidth && invalidRowsBehavior == InvalidFixedWidthRowsBehavior.DROP) {
      return;
    }

    for (int i = 0; i < layoutEntries.size(); ++i) {
      decodingProblemAggregator.setRowColumn(sourceLineNumber, i);

      var entry = layoutEntries.get(i);
      var builder = builders.get(i);

      var startPosition = Math.min(lineLength, entry.start());
      var endPosition = Math.min(lineLength, entry.end());
      var actualWidth = endPosition - startPosition;
      String value = decodeSubarray(startPosition, actualWidth);

      if (entry.end() > lineLength) {
        assert invalidRowsBehavior == InvalidFixedWidthRowsBehavior.KEEP;
      }

      if (emptyToNull && value.trim().isEmpty()) {
        builder.append(null);
      } else {
        builder.append(value);
      }

      context.safepoint();
    }

    tableRowNumber++;
  }

  private String decodeSubarray(int start, int width) throws IOException {
    var baos = new ByteArrayInputStream(readBuffer, start, width);
    var reportingStreamDecoder =
        new ReportingStreamDecoder(baos, charset, decodingProblemAggregator, false);
    return reportingStreamDecoder.readAllIntoMemory();
  }

  private int readLine(PushbackInputStream inputStream) throws IOException {
    if (lineEnding != null && lineEnding.isEmpty()) {
      return readLineByLength(inputStream);
    } else {
      return readLineByEnding(inputStream);
    }
  }

  /**
   * Reads a line into the buffer. The line's end is specified by the layout width, and there are no
   * line endings in the data. Returns the actual length of the entire line, even if that is not
   * equal to `layoutWidth`.
   *
   * <p>Returns -1 if the first read attempt is EOF.
   */
  private int readLineByLength(PushbackInputStream inputStream) throws IOException {
    Context context = Context.getCurrent();

    assert layoutWidth > 0 : "Layout width must be specified for reading by length";

    for (int i = 0; i < layoutWidth; ++i) {
      int c = inputStream.read();
      if (c == -1) {
        if (i == 0) {
          // First attempt was EOF, so return -1 to signify that the stream is done.
          return -1;
        } else {
          // We read some data, but it was not enough to fill the line.
          return i;
        }
      }
      readBuffer[i] = (byte) c;

      context.safepoint();
    }

    return layoutWidth;
  }

  /**
   * Reads a line into the buffer. The line's end is specified by a predefined or inferreed line
   * ending. Returns the actual length of the entire line, even if that is not equal to
   * `layoutWidth`.
   *
   * <p>Returns -1 if the first read attempt is EOF.
   */
  private int readLineByEnding(PushbackInputStream inputStream) throws IOException {
    Context context = Context.getCurrent();

    int currentLineLength = 0;

    while (true) {
      int c = inputStream.read();
      if (c == -1) {
        if (currentLineLength == 0) {
          // First attempt was EOF, so return -1 to signify that the stream is done.
          return -1;
        } else {
          break;
        }
      } else if (lineEnding != null
          && lineEnding.isEmpty()
          && currentLineLength == lineLength - 1) {
        readBuffer[currentLineLength++] = (byte) c;
        break;
      } else if (isLineEnding((byte) c, inputStream)) {
        // Line is done. Don't include the line ending.
        break;
      } else {
        if (currentLineLength >= MAXIMUM_LINE_LENGTH) {
          throw new FixedWidthLineTooLongException(sourceLineNumber, MAXIMUM_LINE_LENGTH);
        }

        assert currentLineLength <= readBuffer.length;

        if (currentLineLength == readBuffer.length) {
          readBuffer = Arrays.copyOf(readBuffer, readBuffer.length * 2);
        }

        readBuffer[currentLineLength] = (byte) c;
        currentLineLength++;
      }

      context.safepoint();
    }

    return currentLineLength;
  }

  // If a line ending is found, it is consumed. If not, anything read beyond the
  // first character 'c' is unconsumed.
  private boolean isLineEnding(byte c, PushbackInputStream inputStream) throws IOException {
    if (lineEnding == null) {
      // Infer -- can be \n, \r or \r\n
      if (c == '\r') {
        var c2 = inputStream.read();
        if (c2 != '\n') {
          inputStream.unread(c2);
        }
        return true;
      } else if (c == '\n') {
        return true;
      } else {
        return false;
      }
    } else {
      // We have a fixed string.

      assert lineEnding.length() > 0
          : "Internal error: should not try to detect the zero-length line ending";

      if (c != lineEnding.charAt(0)) {
        return false;
      }

      for (int i = 1; i < lineEnding.length(); ++i) {
        int c2 = inputStream.read();

        // This condition also handles the EOF (c2 == -1) case
        if (c2 != lineEnding.charAt(i)) {
          // Mismatch, un-read all of the peeked chars.

          if (c2 != -1) {
            inputStream.unread(c2);
          }

          for (int j = i - 1; j > 0; --j) {
            inputStream.unread(lineEnding.charAt(j));
          }
          return false;
        }
      }
      return true;
    }
  }

  private PushbackInputStream allocatePushbackStream(InputStream inputStream) {
    // We need to be able to push back at least the length of the longest line
    // ending. If the line ending is empty, we still allocate a pushback stream
    // so we don't have to duplicate code, but we don't push any values back.
    // The buffer must be at least 1, though.
    int pushbackSize = lineEnding == null ? 2 : Math.max(1, lineEnding.length());
    return new PushbackInputStream(inputStream, pushbackSize);
  }

  private Table makeFinalTable() {
    Context context = Context.getCurrent();

    Column[] columns = new Column[builders.size()];
    for (int i = 0; i < builders.size(); i++) {
      String columnName = layoutEntries.get(i).columnName();
      var stringStorage = builders.get(i).seal();

      // We don't expect InvalidFormat to be propagated back to Enso, there is no particular type
      // that we expect, so it can safely be null.
      Value expectedEnsoValueType = Value.asValue(null);
      CommonParseProblemAggregator parseProblemAggregator =
          ParseProblemAggregator.make(problemAggregator, columnName, expectedEnsoValueType);
      var storage = valueParser.parseColumn(stringStorage, parseProblemAggregator);
      columns[i] = new Column(columnName, storage);

      context.safepoint();
    }

    return new Table(columns);
  }

  private static final int INITIAL_ROW_CAPACITY = 100;

  private void initBuilders(int count) {
    builders = new ArrayList<>(count);
    for (int i = 0; i < count; i++) {
      builders.add(constructBuilder(INITIAL_ROW_CAPACITY));
    }
  }

  private BuilderForType<String> constructBuilder(long initialCapacity) {
    return Builder.getForText(TextType.VARIABLE_LENGTH, initialCapacity);
  }

  public record FixedWidthLayoutEntry(int start, int width, String columnName) {
    public int end() {
      return start + width;
    }
  }

  public enum InvalidFixedWidthRowsBehavior {
    /** Discards rows that are too short for the specified fixed-width layout. */
    DROP,

    /**
     * Keeps rows that are too short for the specified fixed-width layout, keeping partial columns,
     * or using empty strings for entirely missing columns.
     */
    KEEP,
  }

  private void inferHeaders(PushbackInputStream inputStream) throws IOException {
    assert lineEnding == null || !lineEnding.isEmpty();

    lineLength = readLine(inputStream);

    assert lineLength <= MAXIMUM_LINE_LENGTH;

    layoutEntries = inferHeadersFromLine(lineLength);
  }

  private List<FixedWidthLayoutEntry> inferHeadersFromLine(int lineLength) throws IOException {
    var entries = new ArrayList<FixedWidthLayoutEntry>();

    switch (justificationForLayoutInference) {
      case LEFT -> {
        var starts = findStarts(lineLength);
        for (int i = 0; i < starts.size(); ++i) {
          var start = starts.get(i);
          var end = i < starts.size() - 1 ? starts.get(i + 1) : lineLength;
          var width = end - start;
          var columnName = decodeSubarray(start, width).trim();
          entries.add(new FixedWidthLayoutEntry(start, width, columnName));
        }
      }
      case RIGHT -> {
        var ends = findEnds(lineLength);
        for (int i = 0; i < ends.size(); ++i) {
          var start = i == 0 ? 0 : ends.get(i - 1) + 1;
          var end = ends.get(i) + 1;
          var width = end - start;
          var columnName = decodeSubarray(start, width).trim();
          entries.add(new FixedWidthLayoutEntry(start, width, columnName));
        }
      }
    }

    if (entries.isEmpty()) {
      throw new NoColumnNamesFoundException();
    }

    return entries;
  }

  private List<Integer> findStarts(int lineLength) {
    var starts = new ArrayList<Integer>();

    boolean inWhitespace = true;
    for (int i = 0; i < lineLength; ++i) {
      var isWhitespace = readBuffer[i] == 32;
      if (inWhitespace && !isWhitespace) {
        starts.add(i);
      }
      inWhitespace = isWhitespace;
    }

    return starts;
  }

  // Ends are inclusive.
  private List<Integer> findEnds(int lineLength) {
    var ends = new ArrayList<Integer>();

    boolean inWhitespace = true;
    for (int i = 0; i < lineLength; ++i) {
      var isWhitespace = readBuffer[i] == 32;
      if (!inWhitespace && isWhitespace) {
        ends.add(i - 1);
      }
      inWhitespace = isWhitespace;
    }
    ends.add(lineLength - 1);

    return ends;
  }

  public enum Justification {
    LEFT,
    RIGHT
  }
}
