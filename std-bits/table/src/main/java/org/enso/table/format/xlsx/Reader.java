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
import java.util.stream.Stream;

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
   * @param mkDate a function converting java-based dates into a format understandable by the
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
   * @param mkDate a function converting java-based dates into a format understandable by the
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
      Function<LocalDate, Value> mkDate)
      throws IOException {
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
    var deduplicatedColumnNames = NameDeduplicator.deduplicate(colNames);

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
          builders.get(j - minCol).append(getCellValue(cell));
        }
      }
    }
    Column[] columns =
        IntStream.range(0, maxCol - minCol)
            .mapToObj(idx -> new Column(deduplicatedColumnNames.get(idx), builders.get(idx).seal()))
            .toArray(Column[]::new);
    return new Table(columns);
  }

  private static Table readSheetToTable(Sheet sheet, Range range, int skipRows, int rowCount) {
    // Row Range
    int firstRow = sheet.getFirstRowNum() + 1;
    int lastRow = sheet.getLastRowNum() + 1;
    int startRow = (range == null || range.isWholeColumn() ? 1 : range.getTopRow()) + skipRows;
    int endRow =
        Math.min(
            range == null || range.isWholeColumn() ? lastRow : range.getBottomRow(),
            startRow + rowCount - 1);

    // Columns
    int startCol = (range == null || range.isWholeRow() ? 1 : range.getLeftColumn());
    int endCol = (range == null || range.isWholeRow() ? -1 : range.getRightColumn());
    List<Builder> builders =
        endCol == -1
            ? new ArrayList<>()
            : IntStream.range(startCol, endCol + 1)
                .mapToObj(i -> new InferredBuilder(endRow - startRow + 1))
                .collect(Collectors.toList());

    // Read Cell Data
    for (int row = startRow; row <= endRow; row++) {
      if (row < firstRow || row > lastRow) {
        builders.forEach(b -> b.append(null));
      } else {
        Row currentRow = sheet.getRow(row - 1);

        int currentEndCol = endCol == -1 ? currentRow.getLastCellNum() + 1 : endCol;
        for (int i = builders.size(); i <= currentEndCol - startCol; i++) {
          Builder builder = new InferredBuilder(endRow - startRow + 1);
          builder.appendNulls(row - startRow);
          builders.add(builder);
        }

        int firstCol = currentRow.getFirstCellNum() + 1;
        int lastCol = currentRow.getLastCellNum();
        for (int col = startCol; col <= currentEndCol; col++) {
          Object value =
              col < firstCol || col > lastCol ? null : getCellValue(currentRow.getCell(col - 1));
          builders.get(col - startCol).append(value);
        }
      }
    }

    // Create Table
    Column[] columns =
        IntStream.range(0, builders.size())
            .mapToObj(
                idx ->
                    new Column(
                        CellReference.convertNumToColString(startCol + idx),
                        builders.get(idx).seal()))
            .toArray(Column[]::new);

    return new Table(columns);
  }

  private static String getRefersTo(Workbook workbook, String rangeName) {
    for (Name name : workbook.getAllNames()) {
      if (name.getNameName().equalsIgnoreCase(rangeName)) {
        return name.getRefersToFormula();
      }
    }
    return null;
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

  public static String[] readSheetNames(InputStream stream) throws IOException {
    XSSFWorkbook workbook = new XSSFWorkbook(stream);
    int sheetCount = workbook.getNumberOfSheets();
    var output = new String[sheetCount];
    for (int i = 0; i < sheetCount; i++) {
      output[i] = workbook.getSheetName(i);
    }
    return output;
  }

  public static String[] readRangeNames(InputStream stream) throws IOException {
    Workbook workbook = new XSSFWorkbook(stream);
    return workbook.getAllNames().stream().map(Name::getNameName).toArray(String[]::new);
  }

  public static Table readSheetByName(
      InputStream stream, String sheetName, Integer skip_rows, Integer row_limit)
      throws IOException, IllegalArgumentException {
    Workbook workbook = new XSSFWorkbook(stream);

    int sheetIndex = getSheetIndex(workbook, sheetName);
    if (sheetIndex == -1) {
      throw new IllegalArgumentException("Unknown sheet '" + sheetName + "'.");
    }

    Sheet sheet = workbook.getSheetAt(sheetIndex);
    return readSheetToTable(sheet, null, skip_rows == null ? 0 : skip_rows, row_limit == null ? Integer.MAX_VALUE : row_limit);
  }

  public static Table readSheetByIndex(
      InputStream stream, int index, Integer skip_rows, Integer row_limit)
      throws IOException, IllegalArgumentException {
    XSSFWorkbook workbook = new XSSFWorkbook(stream);

    int sheetCount = workbook.getNumberOfSheets();
    if (index < 1 || index > sheetCount) {
      throw new IllegalArgumentException(
          "Sheet index is not in valid range (1 to " + sheetCount + " inclusive).");
    }

    Sheet sheet = workbook.getSheetAt(index - 1);
    return readSheetToTable(sheet, null, skip_rows == null ? 0 : skip_rows, row_limit == null ? Integer.MAX_VALUE : row_limit);
  }

  public static Table readRange(
      InputStream stream, String nameOrAddress, Integer skip_rows, Integer row_limit)
      throws IOException {
    XSSFWorkbook workbook = new XSSFWorkbook(stream);

    String refersTo = getRefersTo(workbook, nameOrAddress);
    if (refersTo == null) {
      refersTo = nameOrAddress;
    }
    Range range = new Range(refersTo);

    int sheetIndex = getSheetIndex(workbook, range.getSheetName());
    if (sheetIndex == -1) {
      throw new IllegalArgumentException("Unknown sheet '" + range.getSheetName() + "'.");
    }

    Sheet sheet = workbook.getSheetAt(sheetIndex);
    return readSheetToTable(sheet, range, skip_rows == null ? 0 : skip_rows, row_limit == null ? Integer.MAX_VALUE : row_limit);
  }
}
