package org.enso.table.read;

import com.univocity.parsers.csv.CsvFormat;
import com.univocity.parsers.csv.CsvParser;
import com.univocity.parsers.csv.CsvParserSettings;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.enso.table.data.column.builder.string.StorageBuilder;
import org.enso.table.data.column.builder.string.StringStorageBuilder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.DefaultIndex;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.util.NameDeduplicator;

public class DelimitedReader {
  public enum HeaderBehavior {
    INFER,
    USE_FIRST_ROW_AS_HEADERS,
    GENERATE_HEADERS
  }

  private final char delimiter;
  private final char quoteCharacter;
  private final char quoteEscapeCharacter;
  private final HeaderBehavior headerBehavior;
  private final long skipRows;
  private final long rowLimit;
  private final int maxColumns;
  private final List<ParsingProblem> warnings = new ArrayList<>();
  private final CsvParser parser;
  private final boolean keepInvalidRows;
  private final boolean warningsAsErrors;

  private static final char noQuoteCharacter = '\0';

  boolean hasQuote() {
    return quoteCharacter != noQuoteCharacter;
  }

  public DelimitedReader(
      InputStream inputStream,
      String delimiter,
      String quote,
      String quoteEscape,
      HeaderBehavior headerBehavior,
      long skipRows,
      long rowLimit,
      int maxColumns,
      boolean keepInvalidRows,
      boolean warningsAsErrors) {
    if (delimiter.isEmpty()) {
      throw new IllegalArgumentException("Empty delimiters are not supported.");
    }
    if (delimiter.length() > 1) {
      throw new IllegalArgumentException(
          "Delimiters consisting of multiple characters or code units are not supported.");
    }

    this.delimiter = delimiter.charAt(0);

    if (quote != null) {
      if (quote.isEmpty()) {
        throw new IllegalArgumentException(
            "Empty quotes are not supported. Set the quote to `Nothing` to disable quotes.");
      }
      if (quote.length() > 1) {
        throw new IllegalArgumentException(
            "Quotes consisting of multiple characters or code units are not supported.");
      }

      quoteCharacter = quote.charAt(0);
      if (quoteCharacter == noQuoteCharacter) {
        throw new IllegalArgumentException("Illegal quote character.");
      }
    } else {
      quoteCharacter = noQuoteCharacter;
    }

    if (quoteEscape != null) {
      if (quoteEscape.isEmpty()) {
        throw new IllegalArgumentException(
            "Empty quote escapes are not supported. Set the escape to `Nothing` to disable escaping quotes.");
      }
      if (quoteEscape.length() > 1) {
        throw new IllegalArgumentException(
            "Quote escapes consisting of multiple characters or code units are not supported.");
      }

      quoteEscapeCharacter = quoteEscape.charAt(0);
    } else {
      quoteEscapeCharacter = noQuoteCharacter;
    }

    this.headerBehavior = headerBehavior;
    this.skipRows = skipRows;
    this.rowLimit = rowLimit;
    this.maxColumns = maxColumns;
    this.keepInvalidRows = keepInvalidRows;
    this.warningsAsErrors = warningsAsErrors;

    parser = setupCsvParser(inputStream);
  }

  private CsvParser setupCsvParser(InputStream inputStream) {
    CsvParserSettings settings = new CsvParserSettings();
    settings.setHeaderExtractionEnabled(false);
    CsvFormat format = new CsvFormat();
    format.setDelimiter(delimiter);
    format.setQuote(quoteCharacter);
    format.setQuoteEscape(quoteEscapeCharacter);
    settings.setFormat(format);
    settings.setMaxCharsPerColumn(-1);
    settings.setMaxColumns(maxColumns);
    settings.setSkipEmptyLines(false);
    settings.setKeepQuotes(true);
    CsvParser parser = new CsvParser(settings);
    parser.beginParsing(inputStream);
    return parser;
  }

  private String parseCell(String cell) {
    if (cell == null) return null;

    // TODO [RW] here we can plug-in some more complex parsing logic, to be done
    //  as part of https://www.pivotaltracker.com/story/show/181824146
    if (cell.isEmpty()) return cell;
    if (cell.charAt(0) == quoteCharacter) {
      return stripQuotes(cell);
    }

    return cell;
  }

  private String parseHeader(String cell) {
    if (cell == null) return "Column";

    if (cell.isEmpty()) return cell;
    if (cell.charAt(0) == quoteCharacter) {
      return stripQuotes(cell);
    }

    return cell;
  }

  private String stripQuotes(String cell) {
    assert cell.charAt(0) == quoteCharacter;

    if (cell.length() < 2 || cell.charAt(cell.length() - 1) != quoteCharacter) {
      reportMismatchedQuote();
      return cell.substring(1);
    } else {
      // Strip quotes.
      return cell.substring(1, cell.length() - 1);
    }
  }

  private void reportMismatchedQuote() {
    reportProblem(new MismatchedQuote());
  }

  private void reportInvalidRow(long source_row, Long table_index, String[] row) {
    reportProblem(new InvalidRow(source_row, table_index, row));
  }

  public List<ParsingProblem> getReportedProblems() {
    return warnings;
  }

  private void reportProblem(ParsingProblem problem) {
    if (warningsAsErrors) {
      throw new ParsingFailedException(problem);
    } else {
      warnings.add(problem);
    }
  }

  private long target_table_index = 0;
  private long current_line = 0;

  private String[] nextRow() {
    current_line = parser.getContext().currentLine() + 1;
    return parser.parseNext();
  }

  public Table read() {
    List<String> headerNames;
    String[] currentRow = nextRow();

    // Skip the first N rows.
    for (long i = 0; currentRow != null && i < skipRows; ++i) {
      currentRow = nextRow();
    }

    // If there are no rows to even infer the headers, we return an empty table.
    if (currentRow == null) {
      return new Table(new Column[0]);
    }

    switch (headerBehavior) {
      case INFER:
        throw new IllegalStateException("Inferring headers is not yet implemented");
      case USE_FIRST_ROW_AS_HEADERS:
        List<String> preprocessedHeaders =
            Arrays.stream(currentRow).map(this::parseHeader).collect(Collectors.toList());
        headerNames = NameDeduplicator.deduplicate(preprocessedHeaders, "_");
        // We have 'used up' the first row, so we load a next one.
        currentRow = nextRow();
        break;
      case GENERATE_HEADERS:
        headerNames = new ArrayList<>(currentRow.length);
        for (int i = 0; i < currentRow.length; ++i) {
          headerNames.add("Column_" + (i + 1));
        }
        break;
      default:
        throw new IllegalStateException("Impossible branch.");
    }

    StorageBuilder[] builders = initBuilders(headerNames.size());

    while (currentRow != null && (rowLimit < 0 || target_table_index < rowLimit)) {
      if (currentRow.length != builders.length) {
        reportInvalidRow(
            current_line,
            keepInvalidRows ? target_table_index : null,
            currentRow);

        if (keepInvalidRows) {
          for (int i = 0; i < builders.length && i < currentRow.length; i++) {
            String item = parseCell(currentRow[i]);
            builders[i] = builders[i].parseAndAppend(item);
          }

          // If the current row had less columns than expected, nulls are inserted for the missing
          // values.
          // If it had more columns, the excess columns are discarded.
          for (int i = currentRow.length; i < builders.length; i++) {
            builders[i] = builders[i].parseAndAppend(null);
          }

          target_table_index++;
        }
      } else {
        for (int i = 0; i < builders.length; i++) {

          String item = parseCell(currentRow[i]);
          builders[i] = builders[i].parseAndAppend(item);
        }

        target_table_index++;
      }

      currentRow = nextRow();
    }

    // FIXME [RW] check if this should be a try-finally
    parser.stopParsing();

    Column[] columns = new Column[builders.length];
    for (int i = 0; i < builders.length; i++) {
      Storage col = builders[i].seal();
      columns[i] = new Column(headerNames.get(i), new DefaultIndex(col.size()), col);
    }
    return new Table(columns);
  }

  private StorageBuilder[] initBuilders(int count) {
    StorageBuilder[] res = new StorageBuilder[count];
    for (int i = 0; i < count; i++) {
      res[i] = new StringStorageBuilder();
    }
    return res;
  }
}
