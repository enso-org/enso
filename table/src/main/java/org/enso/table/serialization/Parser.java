package org.enso.table.serialization;

import com.univocity.parsers.csv.CsvParser;
import com.univocity.parsers.csv.CsvParserSettings;
import org.enso.table.data.column.Column;
import org.enso.table.data.column.builder.ColumnBuilder;
import org.enso.table.data.column.builder.PrimInferredColumnBuilder;
import org.enso.table.data.table.Table;

import java.io.InputStream;

public class Parser {
  private final boolean hasHeader;
  private final String unnamedColumnPrefix;

  private Parser(boolean hasHeader, String unnamedColumnPrefix) {
    this.hasHeader = hasHeader;
    this.unnamedColumnPrefix = unnamedColumnPrefix;
  }

  public static Parser create(boolean hasHeader, String unnamedColumnPrefix) {
    return new Parser(hasHeader, unnamedColumnPrefix);
  }

  public Table parse(InputStream inputStream) {
    CsvParserSettings settings = new CsvParserSettings();
    settings.setHeaderExtractionEnabled(hasHeader);
    settings.detectFormatAutomatically();
    CsvParser parser = new CsvParser(settings);
    parser.beginParsing(inputStream);
    ColumnBuilder[] builders = null;
    String[] header = parser.getContext().headers();
    // TODO[MK] Handle irregular table sizes
    if (header != null) {
      builders = initBuilders(header.length);
    }
    String[] row = parser.parseNext();
    if (row == null) {
      return new Table(new Table.TableColumn[0]);
    }
    if (builders == null) {
      builders = initBuilders(row.length);
    }
    for (int i = 0; i < builders.length; i++) {
      builders[i] = builders[i].parseAndAppend(handleNa(row[i]));
    }
    while ((row = parser.parseNext()) != null) {
      for (int i = 0; i < builders.length; i++) {
        builders[i] = builders[i].parseAndAppend(handleNa(row[i]));
      }
    }
    Table.TableColumn[] columns = new Table.TableColumn[builders.length];
    for (int i = 0; i < builders.length; i++) {
      String name = header != null ? header[i] : unnamedColumnPrefix + i;
      Column col = builders[i].seal();
      columns[i] = new Table.TableColumn(name, col);
    }
    return new Table(columns);
  }

  private ColumnBuilder[] initBuilders(int count) {
    ColumnBuilder[] res = new ColumnBuilder[count];
    for (int i = 0; i < count; i++) {
      res[i] = new PrimInferredColumnBuilder();
    }
    return res;
  }

  private String handleNa(String raw) {
    if (raw == null || raw.length() == 0) {
      return null;
    }
    return raw;
  }
}
