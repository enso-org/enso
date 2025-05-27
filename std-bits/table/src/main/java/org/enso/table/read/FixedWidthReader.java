package org.enso.table.read;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.enso.base.encoding.ReportingStreamDecoder;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.Storage;
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
  private Justification justification,
  private final Charset charset;
  private final long skipRows;
  private final long rowLimit;
  private InvalidFixedWidthRowsBehavior invalidRowsBehavior;
  private boolean emptyToNull;
  private DatatypeParser valueParser;
  private final FixedWidthDecodingProblemAggregator decodingProblemAggregator;
  private FixedWidthReaderProblemAggregator problemAggregator;

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
      Justification justification,
      Charset charset,
      long skipRows,
      long rowLimit,
      InvalidFixedWidthRowsBehavior invalidRowsBehavior,
      boolean emptyToNull,
      DatatypeParser valueParser,
      boolean warningsAsErrors,
      FixedWidthDecodingProblemAggregator decodingProblemAggregator,
      ProblemAggregator problemAggregator) {
    
    assert layoutEntries == null || justificaiton == null : "Exactly one of 'layoutEntries' and 'justification' can be specified";

    this.layoutEntries = layoutEntries;
    this.charset = charset;
    this.skipRows = skipRows;
    this.rowLimit = rowLimit;
    this.invalidRowsBehavior = invalidRowsBehavior;
    this.emptyToNull = emptyToNull;
    this.valueParser = valueParser;
    this.decodingProblemAggregator = decodingProblemAggregator;
    this.problemAggregator =
        new FixedWidthReaderProblemAggregator(
            problemAggregator, invalidRowsBehavior, warningsAsErrors);

    if (layoutEntires != null && layoutEntries.isEmpty()) {
      throw new IllegalArgumentException("Must specify at least one column");
    }

    if (layoutEntries == null) {
      inferHeaders();
    }

    layoutWidth = layoutEntries.get(layoutEntries.size() - 1).end();
  }

  public Table read(InputStream inputStream) throws IOException {
    initBuilders(layoutEntries.size());

    while (true) {
      int lineLength = readLine(inputStream);

      if (lineLength == -1 || (rowLimit != -1 && tableRowNumber >= rowLimit)) {
        break;
      }

      if (sourceLineNumber >= skipRows) {
        addRow(readBuffer, lineLength);
      }

      sourceLineNumber++;
    }

    return makeFinalTable();
  }

  // lineLength is the length of the actual line from the input stream, which
  // might be larger than minimumLineLength and the buffer capacity.
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

    if (lineLength < minimumLineLength) {
      var trn = invalidRowsBehavior == InvalidFixedWidthRowsBehavior.KEEP ? tableRowNumber : null;
      problemAggregator.reportShortLine(sourceLineNumber, trn, lineLength, minimumLineLength);
    }

    if (lineLength < minimumLineLength
        && invalidRowsBehavior == InvalidFixedWidthRowsBehavior.DROP) {
      return;
    }

    for (int i = 0; i < layoutEntries.size(); ++i) {
      decodingProblemAggregator.setRowColumn(sourceLineNumber, i);

      var entry = layoutEntries.get(i);
      var builder = builders.get(i);

      var startPosition = Math.min(lineLength, entry.start());
      var endPosition = Math.min(lineLength, entry.end());
      var actualWidth = endPosition - startPosition;
      String value = decodeSubarray(readBuffer, startPosition, actualWidth);

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

  private String decodeSubarray(int start, int width) {
      var baos = new ByteArrayInputStream(readBuffer, start, width);
      var reportingStreamDecoder =
          new ReportingStreamDecoder(baos, charset, decodingProblemAggregator, false);
      String value = reportingStreamDecoder.readAllIntoMemory();
  }

  /*
   * Reads up to `minimumLineLength` bytes into the buffer. Returns the actual
   * length of the entire line, even if that is not equal to
   * `minimumLineLength`.
   * Returns -1 if the first read attempt is EOF.
   */
  private int readLine(InputStream inputStream) throws IOException {
    Context context = Context.getCurrent();

    int lineLength = 0;

    while (true) {
      int c = inputStream.read();
      if (c == -1) {
        if (lineLength == 0) {
          // First attempt was EOF, so return -1 to signify that the stream is done.
          return -1;
        } else {
          break;
        }
      } else if (c == '\n') {
        // Line is done. Don't include the newline.
        break;
      } else {
        if (lineLength >= MAXIMUM_LINE_LENGTH) {
          throw new FixedWidthLineTooLongException("Fixed width file line " + sourceLineNumber + " is longer than the maximum length of " + MAXIMUM_LINE_LENGTH);
        }

        assert lineLength <= readBuffer.length;

        if (lineLength == readBuffer.length) {
          readBuffer = Arrays.copyOf(readBuffer, readBuffer.length * 2);
        }

        readBuffer[lineLength] = (byte) c;
        lineLength++;
      }

      context.safepoint();
    }

    return lineLength;
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
      Storage<?> storage = valueParser.parseColumn(stringStorage, parseProblemAggregator);
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
     * Keeps rows that are too short for the specified fixed-width layout, keeping partial columns, or
     * using empty strings for entirely missing columns.
     */
    KEEP,
  }

  private void inferHeaders() {
    int lineLength = readLine(inputStream);
  }

  private List<FixedWidthLayoutEntry> inferHeadersFromLine(Justificiaton justification, int lineLength) {
      var entries = new ArrayList<FixedWidthLayoutEntry>();

      switch (justificaiton) {
          case LEFT -> {
              var starts = findStarts(readBuffer, lineLength);
              for (int i = 0; i < starts.length(); ++i) {
                var start = starts.get(i);
                var end = i < starts.length()-1 ? starts.get(i+1) : lineLength;
                var width - end-start;
                var columnName = decodeSubarray(readBuffer, start, width).trim();
                entries.add(new FixedWidthLayoutEntry(start, width, columnName);
              }
          }
          case RIGHT -> {
              var ends = findEnds(readBuffer, lineLength);
              for (int i = 0; i < starts.length(); ++i) {
                var start = i == 0 ? 0 : ends.get(i-1) + 1;
                var end = ends.get(i);
                var width - end-start;
                var columnName = decodeSubarray(readBuffer, start, width).trim();
                entries.add(new FixedWidthLayoutEntry(start, width, columnName);
              }
          }
      }

      if (entries.isEmpty()) {
        throw new NoColumnNamesFoundException();
      }

      return entries;
  }

  private List<Integer> findStarts(int lineLength) {
      var starts = new ArrayList<FixedWidthLayoutEntry>();

      boolean inWhitespace = true;
      for (int i = 0; i < lineLength; ++i) {
          var isWhitespace = readBuffer[i] == 32;
          if (inWhitespace && !isWhitespace) {
              starts.add(i);
          }
          inWhiteSpace = isWhitespace;
      }

      return starts;
  }

  private List<Integer> findEnds(int lineLength) {
      var ends = new ArrayList<FixedWidthLayoutEntry>();

      boolean inWhitespace = true;
      for (int i = 0; i < lineLength; ++i) {
          var isWhitespace = readBuffer[i] == 32;
          if (!inWhitespace && isWhitespace) {
              ends.add(i-1);
          }
          inWhiteSpace = isWhitespace;
      }

      return ends;
  }

  public enum Justification {
    LEFT,
    RIGHT
  }
}
