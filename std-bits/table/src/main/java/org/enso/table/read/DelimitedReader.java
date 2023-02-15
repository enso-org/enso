package org.enso.table.read;

import com.univocity.parsers.csv.CsvFormat;
import com.univocity.parsers.csv.CsvParser;
import com.univocity.parsers.csv.CsvParserSettings;
import org.enso.table.data.column.builder.string.StringStorageBuilder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.error.EmptyFileException;
import org.enso.table.parsing.DatatypeParser;
import org.enso.table.parsing.TypeInferringParser;
import org.enso.table.parsing.problems.AdditionalInvalidRows;
import org.enso.table.parsing.problems.InvalidRow;
import org.enso.table.parsing.problems.MismatchedQuote;
import org.enso.table.parsing.problems.NoOpProblemAggregator;
import org.enso.table.problems.Problem;
import org.enso.table.problems.WithProblems;
import org.enso.table.util.NameDeduplicator;

import java.io.Reader;
import java.util.*;
import java.util.stream.Collectors;

/** A helper for reading delimited (CSV-like) files. */
public class DelimitedReader {

  /**
   * Due to limitations of our CSV parser, we cannot truly disable the comment parsing, so if we want to ensure that a
   * CSV file is parsed without comments enabled we need to set it to some obscure character.
   * <p>
   * The default is to set it to '\0' but that will cause this character (which is not that uncommon, and valid) to act
   * as a start of a comment. However, other parts of the parser may not be dealing best with '\0' anyway, so for now we
   * can keep it. If we want to improve the support for the NULL character, we can select something else here.
   * <p>
   * I considered to  choose `\u0F8EE` which comes from the Private Use Area of the Basic Multilingual Plane. Is has no
   * meaning designated by the Unicode standard.
   */
  public static final char UNUSED_CHARACTER = '\0';

  private static final String COLUMN_NAME = "Column";
  private static final char noQuoteCharacter = '\0';
  private static final long invalidRowsLimit = 10;
  private final char delimiter;
  private final char quoteCharacter;
  private final char quoteEscapeCharacter;
  private final HeaderBehavior headerBehavior;
  private final long skipRows;
  private final long rowLimit;
  private final int maxColumns;
  private final List<Problem> warnings = new ArrayList<>();
  private final CsvParser parser;
  private final DatatypeParser valueParser;
  private final TypeInferringParser cellTypeGuesser;
  private final boolean keepInvalidRows;
  private String newlineSetting;
  private final boolean warningsAsErrors;
  private final NoOpProblemAggregator noOpProblemAggregator = new NoOpProblemAggregator();
  private long invalidRowsCount = 0;
  private long targetTableIndex = 0;
  /** The line number of the start of the current row in the input file. */
  private long currentLine = 0;
  private StringStorageBuilder[] builders = null;

  /**
   * Creates a new reader.
   *
   * @param input a reader providing decoded input characters
   * @param delimiter the delimiter, should be a single character, but is a String for proper
   *     interoperability with Enso; if a string that does not fit in a single character is
   *     provided, an exception is raised
   * @param quote the quote character to use, should be a single character or {@code null}, but is a
   *     String for proper interoperability with Enso; if a string that does not fit in a single
   *     character is provided, an exception is raised
   * @param quoteEscape the quote escape character to use, should be a single character or {@code
   *     null}, but is a * String for proper interoperability with Enso; if a string that does not
   *     fit in a single * character is provided, an exception is raised
   * @param headerBehavior specifies how to set the header for the resulting table
   * @param skipRows specifies how many rows from the input to skip
   * @param rowLimit specifies how many rows to read (does not include the header row)
   * @param maxColumns specifies how many columns can be expected at most
   * @param valueParser an optional parser that is applied to each column to convert it to more
   *     specific datatype
   * @param cellTypeGuesser a helper used to guess cell types, used for the purpose of inferring the
   *     headers, it must not be null if {@code headerBehavior} is set to {@code INFER}.
   * @param keepInvalidRows specifies whether to keep rows that had an unexpected number of columns
   * @param newline specifies what newline character to assume; if set to null, the newline character is autodetected
   * @param commentCharacter specifies what character indicates start of comments; if set to null, comments are disabled
   * @param warningsAsErrors specifies if the first warning should be immediately raised as an error
   *     (used as a fast-path for the error-reporting mode to avoid computing a value that is going
   *     to be discarded anyway)
   */
  public DelimitedReader(
      Reader input,
      String delimiter,
      String quote,
      String quoteEscape,
      HeaderBehavior headerBehavior,
      long skipRows,
      long rowLimit,
      int maxColumns,
      DatatypeParser valueParser,
      TypeInferringParser cellTypeGuesser,
      boolean keepInvalidRows,
      String newline,
      String commentCharacter,
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

    this.valueParser = valueParser;
    this.cellTypeGuesser = cellTypeGuesser;
    this.newlineSetting = newline;
    parser = setupCsvParser(input, commentCharacter);
  }

  /** Creates a {@code CsvParser} according to the settings specified at construction. */
  private CsvParser setupCsvParser(Reader input, String commentCharacter) {
    CsvParserSettings settings = new CsvParserSettings();
    settings.setHeaderExtractionEnabled(false);
    CsvFormat format = new CsvFormat();
    format.setDelimiter(delimiter);
    format.setQuote(quoteCharacter);
    format.setQuoteEscape(quoteEscapeCharacter);
    settings.setMaxCharsPerColumn(-1);
    settings.setMaxColumns(maxColumns);
    settings.setSkipEmptyLines(false);
    settings.setKeepQuotes(true);

    if (newlineSetting == null) {
      settings.setLineSeparatorDetectionEnabled(true);
    } else {
      if (newlineSetting.length() > 2 || newlineSetting.isEmpty()) {
        throw new IllegalArgumentException("The newline sequence should consist of at least 1 and at most 2 characters (codepoints).");
      }
      settings.setLineSeparatorDetectionEnabled(false);
      format.setLineSeparator(newlineSetting);
    }

    if (commentCharacter == null) {
      format.setComment(UNUSED_CHARACTER);
    } else {
      if (commentCharacter.length() != 1) {
        throw new IllegalArgumentException("The comment character should be set to Nothing or consist of exactly one character (codepoint).");
      }

      format.setComment(commentCharacter.charAt(0));
    }

    settings.setFormat(format);
    settings.setNumberOfRowsToSkip(skipRows);
    CsvParser parser = new CsvParser(settings);
    parser.beginParsing(input);
    return parser;
  }

  /** Parses a header cell, removing surrounding quotes (if applicable). */
  private String parseHeader(String cell) {
    if (cell == null) return null;
    return QuoteHelper.stripQuotes(quoteCharacter, this::reportMismatchedQuote, cell);
  }

  private void reportMismatchedQuote() {
    reportProblem(new MismatchedQuote());
  }

  private void reportInvalidRow(long source_row, Long table_index, String[] row) {
    if (invalidRowsCount < invalidRowsLimit) {
      reportProblem(new InvalidRow(source_row, table_index, row));
    }

    invalidRowsCount++;
  }

  /** Returns a list of currently reported problems encountered when parsing the input. */
  private List<Problem> getReportedProblems(List<Problem> nameProblems) {
    List<Problem> result = new ArrayList<>(nameProblems.size() + warnings.size() + 1);
    result.addAll(nameProblems);
    result.addAll(warnings);
    if (invalidRowsCount > invalidRowsLimit) {
      long additionalInvalidRows = invalidRowsCount - invalidRowsLimit;
      result.add(new AdditionalInvalidRows(additionalInvalidRows));
    }
    return result;
  }

  private void reportProblem(Problem problem) {
    if (warningsAsErrors) {
      throw new ParsingFailedException(problem);
    } else {
      warnings.add(problem);
    }
  }

  /**
   * Loads a next row from the CSV file.
   *
   * <p>This is an internal function that just loads the row but does not update the state nor take
   * into consideration pending rows. The regular reading process should use {@code readNextRow}
   * instead.
   */
  private Row loadNextRow() {
    long line = parser.getContext().currentLine() + 1;
    String[] cells = parser.parseNext();
    if (cells == null) return null;
    return new Row(line, cells);
  }

  private record Row(long lineNumber, String[] cells) {}

  private final Queue<Row> pendingRows = new ArrayDeque<>(2);

  /**
   * Reads the next row and updates the current line accordingly. It takes into consideration the
   * pending rows that have already been loaded when inferring the headers but were still not
   * processed.
   *
   * <p>Will return {@code null} if no more rows are available.
   */
  private String[] readNextRow() {
    Row row = pendingRows.isEmpty() ? loadNextRow() : pendingRows.remove();
    if (row == null) {
      return null;
    }

    currentLine = row.lineNumber;
    return row.cells;
  }

  private void appendRow(String[] row) {
    assert builders != null;
    assert canFitMoreRows();

    if (row.length != builders.length) {
      reportInvalidRow(currentLine, keepInvalidRows ? targetTableIndex : null, row);

      if (keepInvalidRows) {
        for (int i = 0; i < builders.length && i < row.length; i++) {
          builders[i] = builders[i].parseAndAppend(row[i]);
        }

        // If the current row had fewer columns than expected, nulls are inserted for the missing
        // values.
        // If it had more columns, the excess columns are discarded.
        for (int i = row.length; i < builders.length; i++) {
          builders[i] = builders[i].parseAndAppend(null);
        }

        targetTableIndex++;
      }
    } else {
      for (int i = 0; i < builders.length; i++) {
        builders[i] = builders[i].parseAndAppend(row[i]);
      }

      targetTableIndex++;
    }
  }

  private boolean canFitMoreRows() {
    return rowLimit < 0 || targetTableIndex < rowLimit;
  }

  private void appendRowIfLimitPermits(String[] row) {
    if (canFitMoreRows()) {
      appendRow(row);
    }
  }

  private WithProblems<List<String>> headersFromRow(String[] row) {
    List<String> preprocessedHeaders =
        Arrays.stream(row).map(this::parseHeader).collect(Collectors.toList());

    NameDeduplicator deduplicator = new NameDeduplicator();
    List<String> names = deduplicator.makeUnique(preprocessedHeaders);
    return new WithProblems<>(names, deduplicator.getProblems());
  }

  private WithProblems<List<String>> generateDefaultHeaders(int columnCount) {
    List<String> headerNames = new ArrayList<>(columnCount);
    for (int i = 0; i < columnCount; ++i) {
      headerNames.add(COLUMN_NAME + "_" + (i + 1));
    }
    return new WithProblems<>(headerNames, Collections.emptyList());
  }

  /**
   * Checks if the given cell contains just plain text that is not null and is not convertible to
   * any more specific type according to the {@code cellTypeGuesser}. This is used for checking the
   * types when inferring the headers.
   */
  private boolean isPlainText(String cell) {
    if (cell == null) return false;
    Object parsed = cellTypeGuesser.parseSingleValue(cell, noOpProblemAggregator);
    return parsed instanceof String;
  }

  /** The column names as defined in the input (if applicable, otherwise null). */
  private String[] definedColumnNames = null;

  /** The effective column names.
   *
   * If {@code GENERATE_HEADERS} is used or if {@code INFER} is used and no headers are found, this will be populated with automatically generated column names. */
  private String[] effectiveColumnNames;

  private List<Problem> headerProblems;

  /** Returns the column names that are defined in the input.
   *
   * Will return {@code null} if {@code GENERATE_HEADERS} is used or if {@code INFER} is used and no headers were found inside of the file. */
  public String[] getDefinedColumnNames() {
    ensureHeadersDetected();
    return definedColumnNames;
  }

  public int getColumnCount() {
    ensureHeadersDetected();
    return effectiveColumnNames.length;
  }

  /** Returns the line separator.
   *
   * If it was provided explicitly at construction, the selected separator is used.
   * If the initial separator was set to {@code null}, the reader tries to detect
   * the separator from file contents.
   */
  public String getEffectiveLineSeparator() {
    if (newlineSetting == null) {
      ensureHeadersDetected();
    }
    return newlineSetting;
  }

  public long getVisitedCharactersCount() {
    ensureHeadersDetected();
    return parser.getContext().currentChar();
  }

  private void ensureHeadersDetected() {
    if (effectiveColumnNames == null) {
      detectHeaders();
    }
  }

  private void detectHeaders() {
    Row firstRow = loadNextRow();

    // Resolve the newline separator:
    if (newlineSetting == null) {
      newlineSetting = parser.getDetectedFormat().getLineSeparatorString();
    }

    if (firstRow == null) {
      effectiveColumnNames = new String[0];
      headerProblems = Collections.emptyList();
      return;
    }

    int expectedColumnCount = firstRow.cells.length;
    boolean wereHeadersDefined = false;
    WithProblems<List<String>> headerNames;

    switch (headerBehavior) {
      case INFER -> {
        Row secondRow = loadNextRow();
        if (secondRow == null) {
          /* If there is only one row in the file, we generate the headers and
           * stop further processing (as nothing more to process). */
          headerNames = generateDefaultHeaders(expectedColumnCount);
          pendingRows.add(firstRow);
        } else {
          assert cellTypeGuesser != null;
          boolean firstAllText = Arrays.stream(firstRow.cells).allMatch(this::isPlainText);
          boolean secondAllText = Arrays.stream(secondRow.cells).allMatch(this ::isPlainText);
          boolean useFirstRowAsHeader = firstAllText && !secondAllText;
          if (useFirstRowAsHeader) {
            headerNames = headersFromRow(firstRow.cells);
            wereHeadersDefined = true;
            pendingRows.add(secondRow);
          } else {
            headerNames = generateDefaultHeaders(expectedColumnCount);
            pendingRows.add(firstRow);
            pendingRows.add(secondRow);
          }
        }
      }
      case USE_FIRST_ROW_AS_HEADERS -> {
        headerNames = headersFromRow(firstRow.cells);
        wereHeadersDefined = true;
      }
      case GENERATE_HEADERS -> {
        headerNames = generateDefaultHeaders(expectedColumnCount);
        pendingRows.add(firstRow);
      }
      default -> throw new IllegalStateException("Impossible branch.");
    }

    headerProblems = headerNames.problems();
    effectiveColumnNames = headerNames.value().toArray(new String[0]);
    if (wereHeadersDefined) {
      definedColumnNames = effectiveColumnNames;
    }
  }

  /** Reads the input stream and returns a Table. */
  public WithProblems<Table> read() {
    ensureHeadersDetected();
    int columnCount = getColumnCount();
    if  (columnCount == 0) {
      throw new EmptyFileException();
    }

    initBuilders(columnCount);
    while (canFitMoreRows()) {
      var currentRow = readNextRow();
      if (currentRow == null) break;
      appendRow(currentRow);
    }

    parser.stopParsing();

    Column[] columns = new Column[builders.length];
    for (int i = 0; i < builders.length; i++) {
      String columnName = effectiveColumnNames[i];
      Storage<String> col = builders[i].seal();

      WithProblems<Storage<?>> parseResult = valueParser.parseColumn(columnName, col);
      for (var problem : parseResult.problems()) {
        reportProblem(problem);
      }
      Storage<?> storage = parseResult.value();

      columns[i] = new Column(columnName, storage);
    }
    return new WithProblems<>(new Table(columns), getReportedProblems(headerProblems));
  }

  private void initBuilders(int count) {
    builders = new StringStorageBuilder[count];
    for (int i = 0; i < count; i++) {
      builders[i] = new StringStorageBuilder();
    }
  }

  /** Specifies how to set the headers for the returned table. */
  public enum HeaderBehavior {
    /** Tries to infer if the headers are present in the file. */
    INFER,

    /** Uses the first row in the file as headers. Duplicate names will be appended suffixes. */
    USE_FIRST_ROW_AS_HEADERS,

    /**
     * Treats the first row as data and generates header names starting with {@code COLUMN_NAME}.
     */
    GENERATE_HEADERS
  }
}
