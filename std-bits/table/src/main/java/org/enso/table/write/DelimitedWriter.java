package org.enso.table.write;

import org.enso.table.data.table.Table;
import org.enso.table.formatting.DataFormatter;
import org.enso.table.problems.WithProblems;

import java.io.IOException;
import java.io.Writer;
import java.util.List;

public class DelimitedWriter {
  private static final char NEWLINE = '\n';
  private final Writer output;
  private final DataFormatter[] columnFormatters;
  private final char delimiter;
  private final String quote;
  private final String quoteEscape;

  private final char quoteChar;
  private final char quoteEscapeChar;

  private final String quoteReplacement;

  private final String quoteEscapeReplacement;
  private final String emptyValue;
  private final WriteQuoteBehavior writeQuoteBehavior;
  private final boolean writeHeaders;

  public DelimitedWriter(
      Writer output,
      DataFormatter[] columnFormatters,
      String delimiter,
      String quote,
      String quoteEscape,
      WriteQuoteBehavior writeQuoteBehavior,
      boolean writeHeaders) {
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
            "Empty quote escapes are not supported. Set the escape to `Nothing` to disable escaping quotes.");
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

    if (this.quoteEscape == this.quote) {
      quoteReplacement = this.quote + "" + this.quote;
      quoteEscapeReplacement = null;
    } else {
      quoteReplacement = this.quoteEscape + "" + this.quote;
      quoteEscapeReplacement = this.quoteEscape + "" + this.quoteEscape;
    }

    this.writeQuoteBehavior = writeQuoteBehavior;
    this.writeHeaders = writeHeaders;
    emptyValue = this.quote + "" + this.quote;
  }

  public void write(Table table) throws IOException {
    int numberOfColumns = table.getColumns().length;
    assert numberOfColumns == columnFormatters.length;

    if (writeHeaders) {
      for (int col = 0; col < numberOfColumns; ++col) {
        boolean isLast = col == numberOfColumns - 1;
        writeCell(table.getColumns()[col].getName(), isLast);
      }
    }

    int numberOfRows = table.rowCount();
    for (int row = 0; row < numberOfRows; ++row) {
      for (int col = 0; col < numberOfColumns; ++col) {
        boolean isLast = col == numberOfColumns - 1;
        Object cellValue = table.getColumns()[col].getStorage().getItemBoxed(row);
        String formatted = columnFormatters[col].format(cellValue);
        writeCell(formatted, isLast);
      }
    }

    output.flush();
  }

  private boolean quotingEnabled() {
    return writeQuoteBehavior != WriteQuoteBehavior.NEVER;
  }

  private void writeCell(String value, boolean isLastInRow) throws IOException {
    String processed = value == null ? "" : quotingEnabled() ? quote(value) : value;
    output.write(processed);
    if (isLastInRow) {
      output.write(NEWLINE);
    } else {
      output.write(delimiter);
    }
  }

  private String quote(String value) {
    if (value.isEmpty()) {
      return emptyValue;
    }

    boolean containsDelimiter = value.indexOf(delimiter) >= 0;
    boolean containsQuote = value.indexOf(quoteChar) >= 0;
    boolean containsQuoteEscape = value.indexOf(quoteEscapeChar) >= 0;

    boolean needsQuoting = containsDelimiter || containsQuote || containsQuoteEscape;

    if (!needsQuoting) {
      return value;
    }

    String escaped;
    if (quoteEscapeChar == quoteChar) {
      escaped = value.replace(quote, quoteReplacement);
    } else {
      escaped = value.replace(quoteEscape, quoteEscapeReplacement).replace(quote, quoteReplacement);
    }

    StringBuilder builder = new StringBuilder(escaped.length() + 2);
    builder.append(quote);
    builder.append(escaped);
    builder.append(quote);
    return builder.toString();
  }
}
