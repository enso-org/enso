package org.enso.table.read;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.ss.util.CellReference;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.InferredBuilder;
import org.enso.table.data.column.storage.ObjectStorage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.excel.ExcelRange;
import org.enso.table.excel.ExcelRow;
import org.enso.table.excel.ExcelSheet;
import org.enso.table.util.NameDeduplicator;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/** A table reader for MS Excel files. */
public class ExcelReader {
  /**
   * Reads a list of sheet names for the specified XLSX/XLS file into an array.
   *
   * @param stream an {@link InputStream} allowing to read the XLSX file contents.
   * @param xls_format specifies whether the file is in Excel Binary Format (95-2003 format).
   * @return a String[] containing the sheet names.
   * @throws IOException when the input stream cannot be read.
   */
  public static String[] readSheetNames(InputStream stream, boolean xls_format) throws IOException {
    Workbook workbook = getWorkbook(stream, xls_format);
    int sheetCount = workbook.getNumberOfSheets();
    var output = new String[sheetCount];
    for (int i = 0; i < sheetCount; i++) {
      output[i] = workbook.getSheetName(i);
    }
    return output;
  }

  /**
   * Reads a list of range names for the specified XLSX/XLS file into an array.
   *
   * @param stream an {@link InputStream} allowing to read the XLSX file contents.
   * @param xls_format specifies whether the file is in Excel Binary Format (95-2003 format).
   * @return a String[] containing the range names.
   * @throws IOException when the input stream cannot be read.
   */
  public static String[] readRangeNames(InputStream stream, boolean xls_format) throws IOException {
    return getWorkbook(stream, xls_format).getAllNames().stream()
        .map(Name::getNameName)
        .toArray(String[]::new);
  }

  /**
   * Reads a sheet by name for the specified XLSX/XLS file into a table.
   *
   * @param stream an {@link InputStream} allowing to read the XLSX file contents.
   * @param sheetName the name of the sheet to read.
   * @param skip_rows skip rows from the top the sheet.
   * @param row_limit maximum number of rows to read.
   * @param xls_format specifies whether the file is in Excel Binary Format (95-2003 format).
   * @return a {@link Table} containing the specified data.
   * @throws IOException when the input stream cannot be read.
   */
  public static Table readSheetByName(
      InputStream stream,
      String sheetName,
      HeaderBehavior headers,
      int skip_rows,
      Integer row_limit,
      boolean xls_format)
      throws IOException, IllegalArgumentException {
    Workbook workbook = getWorkbook(stream, xls_format);

    int sheetIndex = getSheetIndex(workbook, sheetName);
    if (sheetIndex == -1) {
      throw new IllegalArgumentException("Unknown sheet '" + sheetName + "'.");
    }

    return readTable(
        workbook,
        sheetIndex,
        null,
        headers,
        skip_rows,
        row_limit == null ? Integer.MAX_VALUE : row_limit);
  }

  /**
   * Reads a sheet by index for the specified XLSX/XLS file into a table.
   *
   * @param stream an {@link InputStream} allowing to read the XLSX file contents.
   * @param index the 1-based index to the sheet.
   * @param skip_rows skip rows from the top the sheet.
   * @param row_limit maximum number of rows to read.
   * @param xls_format specifies whether the file is in Excel Binary Format (95-2003 format).
   * @return a {@link Table} containing the specified data.
   * @throws IOException when the input stream cannot be read.
   */
  public static Table readSheetByIndex(
      InputStream stream,
      int index,
      HeaderBehavior headers,
      int skip_rows,
      Integer row_limit,
      boolean xls_format)
      throws IOException, IllegalArgumentException {
    Workbook workbook = getWorkbook(stream, xls_format);

    int sheetCount = workbook.getNumberOfSheets();
    if (index < 1 || index > sheetCount) {
      throw new IllegalArgumentException(
          "Sheet index is not in valid range (1 to " + sheetCount + " inclusive).");
    }

    return readTable(
        workbook,
        index - 1,
        null,
        headers,
        skip_rows,
        row_limit == null ? Integer.MAX_VALUE : row_limit);
  }

  /**
   * Reads a range by name or address for the specified XLSX/XLS file into a table.
   *
   * @param stream an {@link InputStream} allowing to read the XLSX file contents.
   * @param rangeNameOrAddress name or address of the range to read.
   * @param skip_rows skip rows from the top of the range.
   * @param row_limit maximum number of rows to read.
   * @param xls_format specifies whether the file is in Excel Binary Format (95-2003 format).
   * @return a {@link Table} containing the specified data.
   * @throws IOException when the input stream cannot be read.
   */
  public static Table readRangeByName(
      InputStream stream,
      String rangeNameOrAddress,
      HeaderBehavior headers,
      int skip_rows,
      Integer row_limit,
      boolean xls_format)
      throws IOException {
    Workbook workbook = getWorkbook(stream, xls_format);

    Name name = workbook.getName(rangeNameOrAddress);
    ExcelRange excelRange = new ExcelRange(name == null ? rangeNameOrAddress : name.getRefersToFormula());
    return readRange(workbook, excelRange, headers, skip_rows, row_limit);
  }

  /**
   * Reads a range for the specified XLSX/XLS file into a table.
   *
   * @param stream an {@link InputStream} allowing to read the XLSX file contents.
   * @param excelRange the range to read.
   * @param skip_rows skip rows from the top of the range.
   * @param row_limit maximum number of rows to read.
   * @param xls_format specifies whether the file is in Excel Binary Format (95-2003 format).
   * @return a {@link Table} containing the specified data.
   * @throws IOException when the input stream cannot be read.
   */
  public static Table readRange(
      InputStream stream,
      ExcelRange excelRange,
      HeaderBehavior headers,
      int skip_rows,
      Integer row_limit,
      boolean xls_format)
      throws IOException {
    return readRange(getWorkbook(stream, xls_format), excelRange, headers, skip_rows, row_limit);
  }

  /** Specifies how to set the headers for the returned table. */
  public enum HeaderBehavior {
    /** Tries to infer if the headers are present in the file. */
    INFER,

    /** Uses the first row in the file as headers. Duplicate names will be appended suffixes. */
    USE_FIRST_ROW_AS_HEADERS,

    /** Uses the default Excel Column Names (e.g. A, B, C). */
    EXCEL_COLUMN_NAMES
  }

  private static Workbook getWorkbook(InputStream stream, boolean xls_format) throws IOException {
    return xls_format ? new HSSFWorkbook(stream) : new XSSFWorkbook(stream);
  }

  private static Table readRange(
      Workbook workbook, ExcelRange excelRange, HeaderBehavior headers, int skip_rows, Integer row_limit) {
    int sheetIndex = getSheetIndex(workbook, excelRange.getSheetName());
    if (sheetIndex == -1) {
      throw new IllegalArgumentException("Unknown sheet '" + excelRange.getSheetName() + "'.");
    }

    return readTable(
        workbook,
        sheetIndex,
        excelRange,
        headers,
        skip_rows,
        row_limit == null ? Integer.MAX_VALUE : row_limit);
  }

  private static Table readTable(
      Workbook workbook,
      int sheetIndex,
      ExcelRange excelRange,
      HeaderBehavior headers,
      int skipRows,
      int rowCount) {
    ExcelSheet sheet = new ExcelSheet(workbook, sheetIndex);

    // Expand Single Cell
    if (excelRange != null && excelRange.isSingleCell()) {
      ExcelRow currentRow = sheet.get(excelRange.getTopRow());
      if (currentRow == null || currentRow.isEmpty(excelRange.getLeftColumn())) {
        return new Table(
            new Column[] {
                new Column(
                    CellReference.convertNumToColString(excelRange.getLeftColumn() - 1),
                    new ObjectStorage(new Object[0], 0)
                )});
      }

      excelRange = expandSingleCell(excelRange, sheet, currentRow);
    }

    // Row Range
    boolean wholeColumn = excelRange == null || excelRange.isWholeColumn();
    int startRow = (wholeColumn ? 1 : excelRange.getTopRow()) + skipRows;
    int endRow = wholeColumn ? sheet.getLastRow() : excelRange.getBottomRow();

    // Column Range
    boolean wholeRow = excelRange == null || excelRange.isWholeRow();
    int startCol = wholeRow ? 1 : excelRange.getLeftColumn();
    int endCol = wholeRow ? -1 : excelRange.getRightColumn();

    // Headers
    NameDeduplicator deduplicator = new NameDeduplicator();
    String[] columnNames = getHeaders(headers, sheet, startRow, startCol, endCol, deduplicator);
    if (columnNames != null) {
      startRow++;
    }

    // Set up Storage
    int size = Math.min(rowCount, endRow - startRow + 1);
    List<Builder> builders =
        wholeRow
            ? new ArrayList<>()
            : IntStream.range(startCol, endCol + 1)
            .mapToObj(i -> new InferredBuilder(size))
            .collect(Collectors.toList());

    // Read Cell Data
    int row = startRow;
    while (row <= endRow && (row - startRow) < rowCount) {
      ExcelRow currentRow = sheet.get(row);
      if (currentRow == null) {
        builders.forEach(b -> b.append(null));
      } else {
        int currentEndCol = endCol == -1 ? Math.max(currentRow.getLastColumn(), startCol + builders.size() - 1) : endCol;
        expandBuilders(builders, size, currentEndCol - startCol, row - startRow);

        for (int col = startCol; col <= currentEndCol; col++) {
          Object value = currentRow.getCellValue(col);
          builders.get(col - startCol).append(value);
        }
      }

      row++;
    }

    // Special case for stopping before firstRow
    if (wholeRow && (rowCount == 0 || row < sheet.getFirstRow())) {
      ExcelRow currentRow = sheet.get(sheet.getFirstRow());
      int currentEndCol = currentRow.getLastColumn();
      expandBuilders(builders, size, currentEndCol - startCol + 1, size);
    }

    // Create Table
    Column[] columns =
        IntStream.range(0, builders.size())
            .mapToObj(
                idx ->
                    new Column(
                        getColumnName(idx + startCol, startCol, columnNames, deduplicator),
                        builders.get(idx).seal()))
            .toArray(Column[]::new);

    return new Table(columns);
  }

  private static ExcelRange expandSingleCell(ExcelRange excelRange, ExcelSheet sheet, ExcelRow currentRow) {
    int bottomRow = excelRange.getTopRow();
    int rightColumn = excelRange.getLeftColumn();
    while (currentRow != null && !currentRow.isEmpty(excelRange.getLeftColumn(), rightColumn)) {
      rightColumn = currentRow.findEndRight(rightColumn);
      bottomRow++;
      currentRow = sheet.get(bottomRow);
    }

    excelRange = new ExcelRange(excelRange.getSheetName(), excelRange.getLeftColumn(), excelRange.getTopRow(), rightColumn, bottomRow - 1);
    return excelRange;
  }

  private static void expandBuilders(List<Builder> builders, int size, int columnCount, int rows) {
    for (int i = builders.size(); i <= columnCount; i++) {
      Builder builder = new InferredBuilder(size);
      builder.appendNulls(rows);
      builders.add(builder);
    }
  }

  private static int getSheetIndex(Workbook workbook, String sheetName) {
    int sheetCount = workbook.getNumberOfSheets();
    for (int i = 0; i < sheetCount; i++) {
      if (workbook.getSheetName(i).equalsIgnoreCase(sheetName)) {
        return i;
      }
    }
    return -1;
  }

  private static String[] getHeaders(HeaderBehavior headers, ExcelSheet sheet, int startRow, int startCol, int endCol, NameDeduplicator deduplicator) {
    return switch (headers) {
      case EXCEL_COLUMN_NAMES -> null;
      case USE_FIRST_ROW_AS_HEADERS -> readRowAsHeaders(sheet.get(startRow), startCol, endCol, deduplicator);
      case INFER -> inferHeaders(sheet.get(startRow), sheet.get(startRow + 1),
          startCol, endCol, deduplicator);
    };
  }

  private static String[] readRowAsHeaders(ExcelRow row, int startCol, int endCol, NameDeduplicator deduplicator) {
    if (row == null) {
      return new String[0];
    }

    int currentEndCol = endCol == -1 ? row.getLastColumn() : endCol;
    DataFormatter formatter = new DataFormatter();

    String[] output = new String[currentEndCol - startCol + 1];
    for (int col = startCol; col <= currentEndCol; col++) {
      Cell cell = row.get(col);

      String name = cell == null ? "" : formatter.formatCellValue(cell);
      if (!name.isEmpty()) {
        name = deduplicator.makeUnique(name);
      }

      output[col - startCol] = name;
    }

    return output;
  }

  private static String[] inferHeaders(ExcelRow row, ExcelRow nextRow, int startCol, int endCol, NameDeduplicator deduplicator) {
    if (row == null || nextRow == null) {
      return null;
    }

    String[] rowNames = getCellsAsText(row, startCol, endCol);
    if (rowNames == null) {
      return null;
    }

    String[] nextNames = getCellsAsText(nextRow, startCol, endCol);
    if (nextNames != null) {
      return null;
    }

    return deduplicator.makeUnique(rowNames);
  }

  private static String[] getCellsAsText(ExcelRow row, int startCol, int endCol) {
    int currentEndCol = endCol == -1 ? row.getLastColumn() : endCol;

    String[] output = new String[currentEndCol - startCol + 1];
    for (int col = startCol; col <= currentEndCol; col++) {
      Cell cell = row.get(col);
      CellType type = ExcelRow.getCellType(cell);
      if (type != CellType._NONE && type != CellType.STRING) {
        return null;
      }
      output[col - startCol] = type == CellType.STRING && cell != null ? cell.getStringCellValue() : "";
    }

    return output;
  }

  private static String getColumnName(int column, int startCol, String[] columnNames, NameDeduplicator deduplicator) {
    if (columnNames == null) {
      return CellReference.convertNumToColString(column - 1);
    }

    int idx = column - startCol;
    String name = idx < columnNames.length ? columnNames[idx] : "";
    if (name == null || name.isEmpty()) {
      name = deduplicator.makeUnique(name);
    }
    return name;
  }
}
