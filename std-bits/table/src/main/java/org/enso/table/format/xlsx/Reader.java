package org.enso.table.format.xlsx;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.Name;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.CellReference;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.InferredBuilder;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.util.NameDeduplicator;
import org.graalvm.polyglot.Value;

import java.io.IOException;
import java.io.InputStream;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/** A table reader for MS Excel files. */
public class Reader {
  /**
   * Reads the specified XLSX file into a table.
   *
   * @param inputStream an {@link InputStream} allowing to read the XLSX file contents.
   * @param sheetIdx specifies which sheet should be read. If the value is a {@link Long}, it is
   *     used as a 0-based index of the sheet. If it is a {@link String}, it is used as a sheet
   *     name. Otherwise, the active sheet is read.
   * @param cellRange specifies a cell range to read from the sheet. If not provided (default), a
   *     range containing all non-empty cells will be selected. If provided, this must be a valid
   *     Excel range address.
   * @param hasHeaders specifies whether the first non-empty row of the sheet should be used for
   *     column names.
   * @param unnamedColumnPrefix specifies the prefix to use for missing columns.
   * @param mkDate a function converting Java-based dates into a format understandable by the
   *     caller.
   * @return a {@link Table} containing the specified data.
   * @throws IOException when the input stream cannot be read.
   */
  public static Table read_xlsx(
      InputStream inputStream,
      Object sheetIdx,
      String cellRange,
      boolean hasHeaders,
      String unnamedColumnPrefix,
      Function<LocalDate, Value> mkDate)
      throws IOException {
    return read_table(
        new XSSFWorkbook(inputStream),
        sheetIdx,
        cellRange,
        hasHeaders,
        unnamedColumnPrefix,
        mkDate);
  }

  /**
   * Reads the specified XLS file into a table.
   *
   * @param is an {@link InputStream} allowing to read the XLS file contents.
   * @param sheetIdx specifies which sheet should be read. If the value is a {@link Long}, it is
   *     used as a 0-based index of the sheet. If it is a {@link String}, it is used as a sheet
   *     name. Otherwise, the active sheet is read.
   * @param cellRange specifies a cell range to read from the sheet. If not provided (default), a
   *     range containing all non-empty cells will be selected. If provided, this must be a valid
   *     Excel range address.
   * @param hasHeaders specifies whether the first non-empty row of the sheet should be used for
   *     column names.
   * @param unnamedColumnPrefix specifies the prefix to use for missing columns.
   * @param mkDate a function converting Java-based dates into a format understandable by the
   *     caller.
   * @return a {@link Table} containing the specified data.
   * @throws IOException when the input stream cannot be read.
   */
  public static Table read_xls(
      InputStream is,
      Object sheetIdx,
      String cellRange,
      boolean hasHeaders,
      String unnamedColumnPrefix,
      Function<LocalDate, Value> mkDate)
      throws IOException {
    return read_table(
        new HSSFWorkbook(is), sheetIdx, cellRange, hasHeaders, unnamedColumnPrefix, mkDate);
  }

  private static Table read_table(
      Workbook workbook,
      Object sheetIdx,
      String cellRange,
      boolean hasHeaders,
      String unnamedColumnPrefix,
      Function<LocalDate, Value> mkDate) {
    Sheet sheet = null;
    if (sheetIdx instanceof Long) {
      sheet = workbook.getSheetAt(((Long) sheetIdx).intValue());
    } else if (sheetIdx instanceof String) {
      sheet = workbook.getSheet((String) sheetIdx);
    }
    if (sheet == null) {
      sheet = workbook.getSheetAt(workbook.getActiveSheetIndex());
    }

    int minRowSpecified, maxRowSpecified, minColSpecified, maxColSpecified;
    if (cellRange != null) {
      var range = CellRangeAddress.valueOf(cellRange);
      minRowSpecified = range.getFirstRow();
      maxRowSpecified = range.getLastRow() == -1 ? Integer.MAX_VALUE : range.getLastRow();
      minColSpecified = range.getFirstColumn();
      maxColSpecified = range.getLastColumn() == -1 ? Integer.MAX_VALUE - 1 : range.getLastColumn();
    } else {
      minRowSpecified = -1;
      maxRowSpecified = Integer.MAX_VALUE;
      minColSpecified = 0;
      maxColSpecified = Integer.MAX_VALUE - 1;
    }

    int minRow = Math.max(sheet.getFirstRowNum(), minRowSpecified);
    int maxRow = Math.min(sheet.getLastRowNum(), maxRowSpecified);
    if (minRow == -1) {
      return new Table(new Column[0]);
    }
    int minCol = Integer.MAX_VALUE;
    int maxCol = -1;
    for (int i = minRow; i <= maxRow; i++) {
      var row = sheet.getRow(i);
      if (row == null) continue;
      var firstCell = row.getFirstCellNum();
      var lastCell = row.getLastCellNum();
      if (firstCell != -1 && lastCell != -1) {
        minCol = Math.min(minCol, firstCell);
        maxCol = Math.max(maxCol, lastCell);
      }
    }
    if (minCol >= maxCol) {
      return new Table(new Column[0]);
    }
    minCol = Math.max(minCol, minColSpecified);
    maxCol = Math.min(maxCol, maxColSpecified + 1);

    List<String> colNames = new ArrayList<>(maxCol - minCol);
    if (hasHeaders) {
      var headerRow = sheet.getRow(minRow);
      minRow++;
      for (int i = minCol; i < maxCol; i++) {
        var cell = headerRow.getCell(i, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL);
        if (cell == null || cell.getCellType() != CellType.STRING) {
          colNames.add(unnamedColumnPrefix + (i - minCol));
        } else {
          colNames.add(cell.getStringCellValue());
        }
      }
    } else {
      for (int i = minCol; i < maxCol; i++) {
        colNames.add(unnamedColumnPrefix + (i - minCol));
      }
    }
    var deduplicatedColumnNames = new NameDeduplicator().makeUnique(colNames);

    List<Builder> builders = new ArrayList<>();
    for (int i = minCol; i < maxCol; i++) {
      builders.add(new InferredBuilder(maxRow - minRow + 1));
    }

    for (int i = minRow; i <= maxRow; i++) {
      var row = sheet.getRow(i);
      for (int j = minCol; j < maxCol; j++) {
        if (row == null) {
          builders.get(j - minCol).append(null);
          continue;
        }
        var cell = row.getCell(j, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL);
        if (cell == null) {
          builders.get(j - minCol).append(null);
        } else {
          Object value = getCellValue(cell);
          if (value instanceof LocalDate) {
            value = mkDate.apply((LocalDate) value);
          }
          builders.get(j - minCol).append(value);
        }
      }
    }
    Column[] columns =
        IntStream.range(0, maxCol - minCol)
            .mapToObj(idx -> new Column(deduplicatedColumnNames.get(idx), builders.get(idx).seal()))
            .toArray(Column[]::new);
    return new Table(columns);
  }

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
      if (row < firstRow || row > lastRow) {
        builders.forEach(b -> b.append(null));
      } else {
        Row currentRow = sheet.getRow(row - 1);
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
