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
  private final String delimiter;
  private final String quote;
  private final String quoteEscape;
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
    this.delimiter = delimiter;
    this.quote = quote;
    this.quoteEscape = quoteEscape;
    this.writeQuoteBehavior = writeQuoteBehavior;
    this.writeHeaders = writeHeaders;
  }

  public WithProblems<Void> write(Table table) throws IOException {
    int numberOfColumns = table.getColumns().length;
    assert numberOfColumns == columnFormatters.length;

    if (writeHeaders) {
      for (int i = 0; i < numberOfColumns; ++i) {
        boolean isLast = i == numberOfColumns - 1;
        writeCell(table.getColumns()[i].getName(), isLast);
      }
    }

    int numberOfRows = table.rowCount();
    for (int i = 0; i < numberOfRows; ++i) {
      for (int j = 0; j < numberOfColumns; ++j) {
        boolean isLast = i == numberOfColumns - 1;
        Object cellValue = table.getColumns()[j].getStorage().getItemBoxed(i);
        String formatted = columnFormatters[j].format(cellValue);
        writeCell(formatted, isLast);
      }
    }

    return new WithProblems<>(null, List.of());
  }

  private void writeCell(String value, boolean isLastInRow) throws IOException {
    String escaped = value; // TODO
    output.write(escaped);
    if (isLastInRow) {
      output.write(NEWLINE);
    } else {
      output.write(delimiter);
    }
  }
}
