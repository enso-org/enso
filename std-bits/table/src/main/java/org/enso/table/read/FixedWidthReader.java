package org.enso.table.read;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;
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
  private InvalidFixedWidthRowsBehavior invalidRowsBehavior;
  private DatatypeParser valueParser;
  private FixedWidthReaderProblemAggregator problemAggregator;

  private List<BuilderForType<String>> builders = null;

  private int minimumLineLength;
  private boolean firstLine = true;
  private int firstLineLength = 0;
  private long sourceLineNumber = 0;
  private long tableRowNumber = 0;

  public FixedWidthReader(
      List<FixedWidthLayoutEntry> layoutEntries,
      InvalidFixedWidthRowsBehavior invalidRowsBehavior,
      DatatypeParser valueParser,
      boolean warningsAsErrors,
      ProblemAggregator problemAggregator) {

    if (layoutEntries.size() == 0) {
      throw new IllegalArgumentException("Must specify at least one column");
    }

    this.layoutEntries = layoutEntries;
    this.invalidRowsBehavior = invalidRowsBehavior;
    this.valueParser = valueParser;
    this.problemAggregator =
        new FixedWidthReaderProblemAggregator(
            problemAggregator, invalidRowsBehavior, warningsAsErrors);

    minimumLineLength = layoutEntries.get(layoutEntries.size() - 1).end();
  }

  public Table read(Reader reader) throws IOException {
    BufferedReader bufferedReader = new BufferedReader(reader);

    initBuilders(layoutEntries.size());

    while (true) {
      String line = bufferedReader.readLine();

      if (line == null) {
        break;
      }

      addRow(line);
    }

    return makeFinalTable();
  }

  private void addRow(String line) {
    if (firstLine) {
      firstLine = false;
      firstLineLength = line.length();
    } else {
      if (line.length() != firstLineLength) {
        problemAggregator.reportInconsistentLineLengths();
      }
    }

    if (line.length() < minimumLineLength) {
      var trn = invalidRowsBehavior == InvalidFixedWidthRowsBehavior.KEEP ? tableRowNumber : null;
      problemAggregator.reportShortLine(
          sourceLineNumber, tableRowNumber, line.length(), minimumLineLength);
    }

    if (line.length() < minimumLineLength
        && invalidRowsBehavior == InvalidFixedWidthRowsBehavior.DROP) {
      sourceLineNumber++;
      return;
    }

    for (int i = 0; i < layoutEntries.size(); ++i) {
      var entry = layoutEntries.get(i);
      var builder = builders.get(i);

      if (entry.end() > line.length()) {
        assert invalidRowsBehavior == InvalidFixedWidthRowsBehavior.KEEP;
        if (entry.start < line.length()) {
          // There is a partial column.
          builders.get(i).append(line.substring(entry.start, line.length()));
        } else {
          // The column is completely off the end.
          builders.get(i).append("");
        }
      } else {
        builders.get(i).append(line.substring(entry.start, entry.end()));
      }
    }

    tableRowNumber++;
    sourceLineNumber++;
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

  public record FixedWidthLayoutEntry(String columnName, int start, int width) {
    public int end() {
      return start + width;
    }
  }
}
