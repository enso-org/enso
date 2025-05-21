package org.enso.table.read;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
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
  private List<FixedWidthLayoutEntry> layoutEntries;
  private final Charset charset;
  private final long rowLimit;
  private InvalidFixedWidthRowsBehavior invalidRowsBehavior;
  private DatatypeParser valueParser;
  private final FixedWidthDecodingProblemAggregator decodingProblemAggregator;
  private FixedWidthReaderProblemAggregator problemAggregator;

  private List<BuilderForType<String>> builders = null;

  private int minimumLineLength;
  private boolean firstLine = true;
  private int firstLineLength = 0;
  private long sourceLineNumber = 0;
  private long tableRowNumber = 0;

  public FixedWidthReader(
      List<FixedWidthLayoutEntry> layoutEntries,
      Charset charset,
      long rowLimit,
      InvalidFixedWidthRowsBehavior invalidRowsBehavior,
      DatatypeParser valueParser,
      boolean warningsAsErrors,
      FixedWidthDecodingProblemAggregator decodingProblemAggregator,
      ProblemAggregator problemAggregator) {

    if (layoutEntries.isEmpty()) {
      throw new IllegalArgumentException("Must specify at least one column");
    }

    this.layoutEntries = layoutEntries;
    this.charset = charset;
    this.rowLimit = rowLimit;
    this.invalidRowsBehavior = invalidRowsBehavior;
    this.valueParser = valueParser;
    this.decodingProblemAggregator = decodingProblemAggregator;
    this.problemAggregator =
        new FixedWidthReaderProblemAggregator(
            problemAggregator, invalidRowsBehavior, warningsAsErrors);

    minimumLineLength = layoutEntries.get(layoutEntries.size() - 1).end();
  }

  public Table read(InputStream inputStream) throws IOException {
    initBuilders(layoutEntries.size());
    byte[] readBuffer = new byte[minimumLineLength];

    while (true) {
      int lineLength = readLine(inputStream, readBuffer);

      if (lineLength == -1 || (rowLimit != -1 && tableRowNumber >= rowLimit)) {
        break;
      }

      addRow(readBuffer, lineLength);
    }

    return makeFinalTable();
  }

  // lineLength is the length of the actual line from the input stream, which
  // might be larger than minimumLineLength and the buffer capacity.
  private void addRow(byte[] line, int lineLength) throws IOException {
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
      sourceLineNumber++;
      return;
    }

    for (int i = 0; i < layoutEntries.size(); ++i) {
      decodingProblemAggregator.setRowColumn(sourceLineNumber, i);

      var entry = layoutEntries.get(i);
      var builder = builders.get(i);

      var startPosition = Math.min(lineLength, entry.start());
      var endPosition = Math.min(lineLength, entry.end());
      var actualWidth = endPosition - startPosition;
      var baos = new ByteArrayInputStream(line, startPosition, actualWidth);
      var reportingStreamDecoder =
          new ReportingStreamDecoder(baos, charset, decodingProblemAggregator, false);
      String value = reportingStreamDecoder.readAllIntoMemory();

      if (entry.end() > lineLength) {
        assert invalidRowsBehavior == InvalidFixedWidthRowsBehavior.KEEP;
      }

      builder.append(value);

      context.safepoint();
    }

    tableRowNumber++;
    sourceLineNumber++;
  }

  /*
   * Reads up to `minimumLineLength` bytes into the buffer. Returns the actual
   * length of the entire line, even if that is not equal to
   * `minimumLineLength`.
   * Returns -1 if the first read attempt is EOF.
   */
  private int readLine(InputStream inputStream, byte[] buffer) throws IOException {
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
        if (lineLength < minimumLineLength) {
          // There is room for the next byte.
          buffer[lineLength] = (byte) c;
        }
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
}
