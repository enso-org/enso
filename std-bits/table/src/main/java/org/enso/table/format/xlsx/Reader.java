package org.enso.table.format.xlsx;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.Name;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.util.CellReference;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.InferredBuilder;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/** A table reader for MS Excel files. */
public class Reader {
  private static Table readSheetToTable(
      Workbook workbook, int sheetIndex, Range range, int skipRows, int rowCount) {
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
      Row currentRow;
      if (row < firstRow || row > lastRow || (currentRow = sheet.getRow(row - 1)) == null) {
        builders.forEach(b -> b.append(null));
      } else {
        int currentEndCol = endCol == -1 ? currentRow.getLastCellNum() + 1 : endCol;
        expandBuilders(builders, size, currentEndCol - startCol, row - startRow);

        int firstCol = currentRow.getFirstCellNum() + 1;
        int lastCol = currentRow.getLastCellNum();
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
      int currentEndCol = currentRow.getLastCellNum() + 1;
      expandBuilders(builders, size, currentEndCol - startCol, size);
    }

    // Create Table
    Column[] columns =
        IntStream.range(0, builders.size())
            .mapToObj(
                idx ->
                    new Column(
                        CellReference.convertNumToColString(startCol + idx - 1),
                        builders.get(idx).seal()))
            .toArray(Column[]::new);

    return new Table(columns);
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
    if (cell == null) {
      return null;
    }

    CellType cellType = cell.getCellType();
    if (cellType == CellType.FORMULA) {
      cellType = cell.getCachedFormulaResultType();
    }

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
      case ERROR:
      case BLANK:
      case _NONE:
        return null;
    }
    return null;
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
      InputStream stream, String sheetName, int skip_rows, Integer row_limit, boolean xls_format)
      throws IOException, IllegalArgumentException {
    Workbook workbook = getWorkbook(stream, xls_format);

    int sheetIndex = getSheetIndex(workbook, sheetName);
    if (sheetIndex == -1) {
      throw new IllegalArgumentException("Unknown sheet '" + sheetName + "'.");
    }

    return readSheetToTable(
        workbook, sheetIndex, null, skip_rows, row_limit == null ? Integer.MAX_VALUE : row_limit);
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
      InputStream stream, int index, int skip_rows, Integer row_limit, boolean xls_format)
      throws IOException, IllegalArgumentException {
    Workbook workbook = getWorkbook(stream, xls_format);

    int sheetCount = workbook.getNumberOfSheets();
    if (index < 1 || index > sheetCount) {
      throw new IllegalArgumentException(
          "Sheet index is not in valid range (1 to " + sheetCount + " inclusive).");
    }

    return readSheetToTable(
        workbook, index - 1, null, skip_rows, row_limit == null ? Integer.MAX_VALUE : row_limit);
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
      int skip_rows,
      Integer row_limit,
      boolean xls_format)
      throws IOException {
    Workbook workbook = getWorkbook(stream, xls_format);

    Name name = workbook.getName(rangeNameOrAddress);
    Range range = new Range(name == null ? rangeNameOrAddress : name.getRefersToFormula());
    return readRange(workbook, range, skip_rows, row_limit);
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
      InputStream stream, Range range, int skip_rows, Integer row_limit, boolean xls_format)
      throws IOException {
    return readRange(getWorkbook(stream, xls_format), range, skip_rows, row_limit);
  }

  private static Workbook getWorkbook(InputStream stream, boolean xls_format) throws IOException {
    return xls_format ? new HSSFWorkbook(stream) : new XSSFWorkbook(stream);
  }

  private static Table readRange(Workbook workbook, Range range, int skip_rows, Integer row_limit) {
    int sheetIndex = getSheetIndex(workbook, range.getSheetName());
    if (sheetIndex == -1) {
      throw new IllegalArgumentException("Unknown sheet '" + range.getSheetName() + "'.");
    }

    return readSheetToTable(
        workbook, sheetIndex, range, skip_rows, row_limit == null ? Integer.MAX_VALUE : row_limit);
  }
}
