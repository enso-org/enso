package org.enso.table.format.csv;

import org.enso.table.data.table.Table;

import java.io.*;
import java.util.Arrays;
import java.util.function.Function;

public class Writer {
  public static String writeString(
      Table table,
      boolean writeHeader,
      boolean forceQuote,
      String newline,
      String separator,
      Function<Object, String> toCsvString)
      throws IOException {
    var builder = new StringBuilder();
    writeAppendable(
        table,
        builder,
        0,
        table.rowCount(),
        writeHeader,
        forceQuote,
        newline,
        separator,
        toCsvString);
    return builder.toString();
  }

  public static void writePath(
      Table table,
      String path,
      Integer maxRecords,
      boolean writeHeader,
      boolean forceQuote,
      String newline,
      String separator,
      Function<Object, String> toCsvString)
      throws IOException {
    var file = new File(path);
    if (maxRecords == null || maxRecords >= table.rowCount()) {
      try (var writer = new PrintWriter(new FileWriter(file))) {
        writeAppendable(
            table,
            writer,
            0,
            table.rowCount(),
            writeHeader,
            forceQuote,
            newline,
            separator,
            toCsvString);
      }
    } else {
      var nfiles = table.rowCount() / maxRecords + (table.rowCount() % maxRecords == 0 ? 0 : 1);
      var originalName = file.getName();
      var extIndex = originalName.indexOf('.');
      var dir = file.getParentFile();
      String basename;
      String extension;
      if (extIndex == -1) {
        basename = originalName;
        extension = "";
      } else {
        basename = originalName.substring(0, extIndex);
        extension = originalName.substring(extIndex);
      }
      for (int i = 0; i < nfiles; i++) {
        var currentFile = new File(dir, basename + "_" + (i + 1) + extension);
        try (var writer = new PrintWriter(new FileWriter(currentFile))) {
          writeAppendable(
              table,
              writer,
              i * maxRecords,
              maxRecords,
              writeHeader,
              forceQuote,
              newline,
              separator,
              toCsvString);
        }
      }
    }
  }

  private static void writeAppendable(
      Table table,
      Appendable buffer,
      int startRow,
      int maxRows,
      boolean writeHeader,
      boolean forceQuote,
      String newline,
      String separator,
      Function<Object, String> toCsvString)
      throws IOException {
    var columns = Arrays.asList(table.getColumns());
    var index = table.getIndex().toColumn();
    if (index != null) {
      columns.add(0, index);
    }
    if (writeHeader) {
      for (int j = 0; j < columns.size(); j++) {
        buffer.append(quoteString(columns.get(j).getName(), separator, forceQuote));
        buffer.append(j < columns.size() - 1 ? separator : newline);
      }
    }
    var rowLimit = Math.min(startRow + maxRows, table.rowCount());
    for (int i = startRow; i < rowLimit; i++) {
      for (int j = 0; j < columns.size(); j++) {
        buffer.append(
            quoteString(
                columns.get(j).getStorage().getCsvString(i, toCsvString), separator, forceQuote));
        buffer.append(j < columns.size() - 1 ? separator : newline);
      }
    }
  }

  private static String quoteString(String value, String separator, boolean forceQuote) {
    var quoteQuotes = value.replaceAll("\"", "\"\"");
    if (forceQuote || value.contains(separator)) {
      return "\"" + quoteQuotes + "\"";
    } else {
      return quoteQuotes;
    }
  }
}
