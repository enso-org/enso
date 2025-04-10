package org.enso.table.read;

import java.io.BufferedReader;
import java.io.Reader;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.error.EmptyFileException;
import org.enso.table.parsing.DatatypeParser;
import org.enso.table.parsing.TypeInferringParser;
import org.enso.table.parsing.problems.CommonParseProblemAggregator;
import org.enso.table.parsing.problems.NoOpParseProblemAggregator;
import org.enso.table.parsing.problems.ParseProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.NameDeduplicator;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public class FixedWidthReader {
  private Object layoutEntries;
  private InvalidRowsBehavior invalidRowsBehavior;
  private DatatypeParser valueParser;

  private int minimumLength;
  private List<BuilderForType<String>> builders = null;

  public FixedWidthReader(
      Object layoutEntries,
      InvalidRowsBehavior invalidRowsBehavior,
      DatatypeParser valueParser,
      boolean warningsAsErrors,
      ProblemAggregator problemAggregator) {
    if (invalidRowsBehavior == InvalidRowsBehavior.ADD_EXTRA_COLUMNS) {
      throw new IllegalArgumentException("FixedWidthReader does not allow InvalidRowsBehavior.ADD_EXTRA_COLUMNS");
    }

    if (layoutEntries.length == 0) {
        throw new IllegalArgumentException("Must specify at least one column");
    }

    this.layoutEntries = layoutEntries;
    this.warningsAsErrors = warningsAsErrors;
    this.problemAggregator = new FixedWidthReaderProblemAggregator(problemAggregator);

    minimumLength = layoutEntries]layoutEntries.length-1].end(); 
  }

  public Table read(Reader reader) {
    BufferedReader bufferedReader = new BufferedReader(reader);

    initBuilders(layoutEntries.length);

    int lineNumber = 1;
    while (true) {
      String line = bufferedReader.readLine();

      if (line == null) {
          break;
      }

      addRow(lineNumber, line);
      lineNumber++;
    }
  }

  private void addRow(int lineNumber, String line) {
    if (line.length() < minimumLength && invalidRowsBehavior == InvalidRowsBehavior.DROP) {
      problemAggregator.reportShortLine(lineNumber, line.length());
      return;
    }

    for (int i = 0; i < layoutEntries.length; ++i) {
      var entry = layoutEntries[i];
      var builder = builders[i];

      if (entry.end() > line.length()) {
        assert invalidRowsBehavior == InvalidRowsBehavior.KEEP;
        builders.get(i).append("");
      }  else {
        builders.get(i).append(line.substring(entry.start, entry.end()));
      }
    }

    return makeFinalTable();
  }

  private void Table makeFinalTable() {
    Context context = Context.getCurrent();

    Column[] columns = new Column[builders.size()];
    for (int i = 0; i < builders.size(); i++) {
      String columnName = layoutEntries[i];
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
