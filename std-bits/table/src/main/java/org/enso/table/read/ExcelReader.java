package org.enso.table.read;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.ss.util.CellReference;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.InferredBuilder;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.format.xlsx.Range;
import org.enso.table.util.NameDeduplicator;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/** A table reader for MS Excel files. */
public class ExcelReader {
  private static Table readSheetToTable(
      Workbook workbook,
      int sheetIndex,
      Range range,
      HeaderBehavior headers,
      int skipRows,
      int rowCount) {
    Sheet sheet = workbook.getSheetAt(sheetIndex);

    // Row Range
    int firstRow = sheet.getFirstRowNum() + 1;
    int lastRow = sheet.getLastRowNum() + 1;
    boolean wholeColumn = range == null || range.isWholeColumn();
    int startRow = (wholeColumn ? 1 : range.getTopRow()) + skipRows;
    int endRow = wholeColumn ? lastRow : range.getBottomRow();

    // Columns
    boolean wholeRow = range == null || range.isWholeRow();
    int startCol = wholeRow ? 1 : range.getLeftColumn();
    int endCol = wholeRow ? -1 : range.getRightColumn();

    // Headers
    NameDeduplicator deduplicator = new NameDeduplicator();
    String[] columnNames = switch (headers) {
      case EXCEL_COLUMN_NAMES -> null;
      case USE_FIRST_ROW_AS_HEADERS ->
        readRowAsHeaders(getRow(startRow, sheet, firstRow, lastRow), startCol, endCol, deduplicator);
      case INFER ->
        inferHeaders(
            getRow(startRow, sheet, firstRow, lastRow),
            getRow(startRow + 1, sheet, firstRow, lastRow),
            startCol, endCol, deduplicator);
    };
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
      Row currentRow = row < firstRow || row > lastRow ? null : sheet.getRow(row - 1);
      if (currentRow == null) {
        builders.forEach(b -> b.append(null));
      } else {
        int firstCol = currentRow.getFirstCellNum() + 1;
        int lastCol = currentRow.getLastCellNum();

        int currentEndCol = endCol == -1 ? Math.max(lastCol, startCol + builders.size() - 1) : endCol;
        expandBuilders(builders, size, currentEndCol - startCol, row - startRow);

        for (int col = startCol; col <= currentEndCol; col++) {
          Object value =
              col < firstCol || col > lastCol ? null : getCellValue(currentRow.getCell(col - 1));
          builders.get(col - startCol).append(value);
        }
      }

      row++;
    }

    // Special case for stopping before firstRow
    if (wholeRow && (rowCount == 0 || row < firstRow)) {
      Row currentRow = sheet.getRow(firstRow - 1);
      int currentEndCol = currentRow.getLastCellNum();
      expandBuilders(builders, size, currentEndCol - startCol, size);
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

  private static Row getRow(int row, Sheet sheet, int firstRow, int lastRow) {
    return row < firstRow || row > lastRow ? null : sheet.getRow(row - 1);
  }

  private static String[] readRowAsHeaders(Row row, int startCol, int endCol, NameDeduplicator deduplicator) {
    if (row == null) {
      return new String[0];
    }

    int currentEndCol = endCol == -1 ? row.getLastCellNum() + 1 : endCol;
    DataFormatter formatter = new DataFormatter();

    int firstCol = row.getFirstCellNum() + 1;
    int lastCol = row.getLastCellNum();

    String[] output = new String[currentEndCol - startCol + 1];
    for (int col = startCol; col <= currentEndCol; col++) {
      String name = col < firstCol || col > lastCol ? "" : formatter.formatCellValue(row.getCell(col - 1));
      if (!name.isEmpty()) {
        name = deduplicator.makeUnique(name);
      }
      output[col - startCol] = name;
    }
    return output;
  }

  private static String[] inferHeaders(Row row, Row nextRow, int startCol, int endCol, NameDeduplicator deduplicator) {
    String[] rowNames = getRowAsStrings(row, startCol, endCol);
    if (rowNames == null) {
      return null;
    }

    String[] nextNames = getRowAsStrings(nextRow, startCol, endCol);
    if (nextNames != null) {
      return null;
    }

    return deduplicator.makeUnique(rowNames);
  }

  private static String[] getRowAsStrings(Row row, int startCol, int endCol) {
    if (row == null) {
      return null;
    }

    int currentEndCol = endCol == -1 ? row.getLastCellNum() : endCol;
    int firstCol = row.getFirstCellNum() + 1;
    int lastCol = row.getLastCellNum();

    String[] output = new String[currentEndCol - startCol + 1];
    for (int col = startCol; col <= currentEndCol; col++) {
      Cell cell = col < firstCol || col > lastCol ? null : row.getCell(col - 1);
      CellType type = getCellType(cell);
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

  private static Object getCellValue(Cell cell) {
    CellType cellType = getCellType(cell);
    switch (cellType) {
      case NUMERIC:
        if (DateUtil.isCellDateFormatted(cell)) {
          return cell.getLocalDateTimeCellValue().toLocalDate();
        } else {
          return cell.getNumericCellValue();
        }
      case STRING:
        return cell.getStringCellValue();
      case BOOLEAN:
        return cell.getBooleanCellValue();
      default:
        return null;
    }
  }

  private static CellType getCellType(Cell cell) {
    if (cell == null) {
      return CellType._NONE;
    }

    CellType cellType = cell.getCellType();
    if (cellType == CellType.FORMULA) {
      cellType = cell.getCachedFormulaResultType();
    }

    return cellType;
  }

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

    return readSheetToTable(
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

    return readSheetToTable(
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
    Range range = new Range(name == null ? rangeNameOrAddress : name.getRefersToFormula());
    return readRange(workbook, range, headers, skip_rows, row_limit);
  }

  /**
   * Reads a range for the specified XLSX/XLS file into a table.
   *
   * @param stream an {@link InputStream} allowing to read the XLSX file contents.
   * @param range the range to read.
   * @param skip_rows skip rows from the top of the range.
   * @param row_limit maximum number of rows to read.
   * @param xls_format specifies whether the file is in Excel Binary Format (95-2003 format).
   * @return a {@link Table} containing the specified data.
   * @throws IOException when the input stream cannot be read.
   */
  public static Table readRange(
      InputStream stream,
      Range range,
      HeaderBehavior headers,
      int skip_rows,
      Integer row_limit,
      boolean xls_format)
      throws IOException {
    return readRange(getWorkbook(stream, xls_format), range, headers, skip_rows, row_limit);
  }

  private static Workbook getWorkbook(InputStream stream, boolean xls_format) throws IOException {
    return xls_format ? new HSSFWorkbook(stream) : new XSSFWorkbook(stream);
  }

  private static Table readRange(
      Workbook workbook, Range range, HeaderBehavior headers, int skip_rows, Integer row_limit) {
    int sheetIndex = getSheetIndex(workbook, range.getSheetName());
    if (sheetIndex == -1) {
      throw new IllegalArgumentException("Unknown sheet '" + range.getSheetName() + "'.");
    }

    return readSheetToTable(
        workbook,
        sheetIndex,
        range,
        headers,
        skip_rows,
        row_limit == null ? Integer.MAX_VALUE : row_limit);
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
}
