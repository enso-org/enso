package org.enso.table.write;

import java.io.IOException;
import java.io.Writer;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import org.enso.table.data.table.Table;
import org.enso.table.data.table.problems.UnquotedCharactersInOutput;
import org.enso.table.formatting.DataFormatter;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.read.DelimitedReader;

public class DelimitedWriter {
  private final String newline;
  private final Writer output;
  private final DataFormatter[] columnFormatters;
  private final char delimiter;
  private final String quote;
  private final String quoteEscape;

  private final char quoteChar;
  private final char quoteEscapeChar;

  private final char commentChar;

  private final String quoteReplacement;

  private final String quoteEscapeReplacement;
  private final String emptyValue;
  private final WriteQuoteBehavior writeQuoteBehavior;
  private final boolean writeHeaders;
  private final ColumnAggregatedProblemAggregator problemAggregator;

  public DelimitedWriter(
      Writer output,
      DataFormatter[] columnFormatters,
      String delimiter,
      String newline,
      String quote,
      String quoteEscape,
      String comment,
      WriteQuoteBehavior writeQuoteBehavior,
      boolean writeHeaders,
      ProblemAggregator problemAggregator) {
    this.problemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);
    this.newline = newline;
    this.output = output;
    this.columnFormatters = columnFormatters;

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

      this.quote = quote;
      this.quoteChar = quote.charAt(0);
    } else {
      this.quote = null;
      this.quoteChar = '\0';
    }

    if (quoteEscape != null) {
      if (quoteEscape.isEmpty()) {
        throw new IllegalArgumentException(
            "Empty quote escapes are not supported. Set the escape to `Nothing` to disable escaping"
                + " quotes.");
      }
      if (quoteEscape.length() > 1) {
        throw new IllegalArgumentException(
            "Quote escapes consisting of multiple characters or code units are not supported.");
      }

      this.quoteEscape = quoteEscape;
      this.quoteEscapeChar = quoteEscape.charAt(0);
    } else {
      this.quoteEscape = null;
      this.quoteEscapeChar = '\0';
    }

    if (this.quote != null) {
      if (this.quote.equals(this.quoteEscape)) {
        quoteReplacement = this.quote + "" + this.quote;
        quoteEscapeReplacement = null;
      } else {
        quoteReplacement = this.quoteEscape + "" + this.quote;
        quoteEscapeReplacement = this.quoteEscape + "" + this.quoteEscape;
      }
    } else {
      quoteReplacement = null;
      quoteEscapeReplacement = null;
    }

    if (comment != null) {
      if (comment.length() != 1) {
        throw new IllegalArgumentException(
            "The comment character must consist of exactly 1 codepoint.");
      }

      commentChar = comment.charAt(0);
    } else {
      commentChar = DelimitedReader.UNUSED_CHARACTER;
    }

    this.writeQuoteBehavior = writeQuoteBehavior;
    this.writeHeaders = writeHeaders;
    emptyValue = this.quote + "" + this.quote;
  }

  public void write(Table table) throws IOException {
    int numberOfColumns = table.getColumns().length;
    assert numberOfColumns == columnFormatters.length;

    if (writeHeaders) {
      boolean quoteAllHeaders = writeQuoteBehavior == WriteQuoteBehavior.ALWAYS;
      for (int col = 0; col < numberOfColumns; ++col) {
        boolean isLast = col == numberOfColumns - 1;
        String columnName = table.getColumns()[col].getName();
        writeCell(columnName, isLast, quoteAllHeaders, columnName, -1);
      }
    }

    int numberOfRows = table.rowCount();
    for (int row = 0; row < numberOfRows; ++row) {
      for (int col = 0; col < numberOfColumns; ++col) {
        boolean isLast = col == numberOfColumns - 1;
        String columnName = table.getColumns()[col].getName();
        Object cellValue = table.getColumns()[col].getStorage().getItemBoxed(row);
        String formatted = columnFormatters[col].format(cellValue);
        boolean wantsQuoting =
            writeQuoteBehavior == WriteQuoteBehavior.ALWAYS && wantsQuotesInAlwaysMode(cellValue);
        writeCell(formatted, isLast, wantsQuoting, columnName, row);
      }
    }

    output.flush();
  }

  private boolean wantsQuotesInAlwaysMode(Object value) {
    return !isNonTextPrimitive(value);
  }

  private boolean isNonTextPrimitive(Object value) {
    return value instanceof Long
        || value instanceof Double
        || value instanceof Boolean
        || value instanceof LocalDate
        || value instanceof LocalDateTime
        || value instanceof LocalTime
        || value instanceof ZonedDateTime;
  }

  private boolean quotingEnabled() {
    return writeQuoteBehavior != WriteQuoteBehavior.NEVER;
  }

  private void writeCell(
      String value, boolean isLastInRow, boolean wantsQuoting, String columnName, int row)
      throws IOException {
    String processed = value == null ? "" : quote(value, wantsQuoting, columnName, row);
    output.write(processed);
    if (isLastInRow) {
      output.write(newline);
    } else {
      output.write(delimiter);
    }
  }

  /**
   * Wraps the value in quotes, escaping any characters if necessary.
   *
   * <p>The {@code wantsQuoting} parameter allows to request quoting even if it wouldn't normally be
   * necessary. This is used to implement the `always_quote` mode for text and custom objects.
   */
  private String quote(String value, boolean wantsQuoting, String columnName, int row) {
    if (value.isEmpty()) {
      return emptyValue;
    }

    boolean containsQuote = false;
    boolean containsQuoteEscape = false;
    boolean containsCharactersThatNeedQuoting = false;

    for (int i = 0; i < value.length(); ++i) {
      char c = value.charAt(i);
      containsQuote |= c == quoteChar;
      containsQuoteEscape |= (quoteEscape != null) && (c == quoteEscapeChar);
      containsCharactersThatNeedQuoting |= containsQuote || containsQuoteEscape;
      containsCharactersThatNeedQuoting |= c == delimiter || c == '\n' || c == '\r';
      /*
       * TODO This should be checking if commenting is enabled, but currently
       * due to limitations of the reader library it is always enabled, just
       * sometimes the comment char is set to `\0`. See the documentation of
       * {@link DelimitedReader#COMMENT_CHARACTER}.
       *
       * See issue https://github.com/enso-org/enso/issues/5655
       */
      containsCharactersThatNeedQuoting |= c == commentChar;

      // Early short-circuit where further iterations will not yield any new information.
      if (containsQuote && containsQuoteEscape) {
        break;
      }
    }

    if (!quotingEnabled()) {
      if (containsCharactersThatNeedQuoting) {
        problemAggregator.reportColumnAggregatedProblem(
            new UnquotedCharactersInOutput(columnName, row));
      }

      return value;
    }

    // Quoting is enabled.

    boolean shouldQuote = wantsQuoting || containsCharactersThatNeedQuoting;
    if (!shouldQuote) {
      // But the particular value does not need to be quoted, and it was not requested to be quoted.
      return value;
    }

    String escaped = value;
    if (quoteEscapeChar != quoteChar && containsQuoteEscape) {
      escaped = escaped.replace(quoteEscape, quoteEscapeReplacement);
    }

    if (containsQuote) {
      escaped = escaped.replace(quote, quoteReplacement);
    }

    StringBuilder builder = new StringBuilder(escaped.length() + 2);
    builder.append(quote);
    builder.append(escaped);
    builder.append(quote);
    return builder.toString();
  }
}
