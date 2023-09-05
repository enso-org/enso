package org.enso.table.write;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Name;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.error.ColumnCountMismatchException;
import org.enso.table.error.ColumnNameMismatchException;
import org.enso.table.error.ExistingDataException;
import org.enso.table.error.InvalidLocationException;
import org.enso.table.error.RangeExceededException;
import org.enso.table.excel.ExcelHeaders;
import org.enso.table.excel.ExcelRange;
import org.enso.table.excel.ExcelRow;
import org.enso.table.excel.ExcelSheet;
import org.enso.table.util.ColumnMapper;
import org.enso.table.util.NameDeduplicator;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Arrays;
import java.util.function.Function;

public class ExcelWriter {
  private static final double SECONDS_IN_A_DAY = 86400.0;

  private static Function<Object, Boolean> ensoToTextCallback;

  public static Function<Object, Boolean> getEnsoToTextCallback() {
    return ensoToTextCallback;
  }

  public static void setEnsoToTextCallbackIfUnset(Function<Object, Boolean> callback) {
    if (ensoToTextCallback == null) {
      ensoToTextCallback = callback;
    }
  }

  public static void writeTableToSheet(Workbook workbook, int sheetIndex, ExistingDataMode existingDataMode, int firstRow, Table table, Long rowLimit, ExcelHeaders.HeaderBehavior headers)
      throws InvalidLocationException, RangeExceededException, ExistingDataException, IllegalStateException, ColumnNameMismatchException, ColumnCountMismatchException {
    if (sheetIndex == 0 || sheetIndex > workbook.getNumberOfSheets()) {
      int i = 1;
      while (workbook.getSheet("Sheet" + i) != null) {
        i++;
      }

      Sheet sheet = workbook.createSheet("Sheet" + i);
      if (sheetIndex == 0) {
        workbook.setSheetOrder(sheet.getSheetName(), 0);
      }

      writeTableToSheet(workbook, sheet, firstRow, 1, table, rowLimit, headers != ExcelHeaders.HeaderBehavior.EXCEL_COLUMN_NAMES);
    } else if (existingDataMode == ExistingDataMode.REPLACE) {
      headers = headers != ExcelHeaders.HeaderBehavior.INFER ? headers :
          shouldWriteHeaders(new ExcelSheet(workbook, sheetIndex), firstRow + 1, 1, -1);

      String sheetName = workbook.getSheetName(sheetIndex - 1);
      workbook.removeSheetAt(sheetIndex - 1);

      Sheet sheet = workbook.createSheet(sheetName);
      workbook.setSheetOrder(sheetName, sheetIndex - 1);
      writeTableToSheet(workbook, sheet, firstRow, 1, table, rowLimit, headers != ExcelHeaders.HeaderBehavior.EXCEL_COLUMN_NAMES);
    } else if (existingDataMode == ExistingDataMode.ERROR){
      throw new ExistingDataException("Sheet already exists, and cannot be replaced in current mode.");
    } else {
      // In Append Mode, so lets go to a Range based approach.
      ExcelRange range = new ExcelRange(workbook.getSheetName(sheetIndex - 1), 1, 1);
      writeTableToRange(workbook, range, existingDataMode, firstRow, table, rowLimit, headers);
    }
  }

  public static void writeTableToSheet(Workbook workbook, String sheetName, ExistingDataMode existingDataMode, int firstRow, Table table, Long rowLimit, ExcelHeaders.HeaderBehavior headers)
      throws InvalidLocationException, RangeExceededException, ExistingDataException, IllegalStateException, ColumnNameMismatchException, ColumnCountMismatchException {
    int sheetIndex = workbook.getNumberOfSheets() == 0 ? -1 : workbook.getSheetIndex(sheetName);
    if (sheetIndex == -1) {
      writeTableToSheet(workbook, workbook.createSheet(sheetName), firstRow, 1, table, rowLimit, headers != ExcelHeaders.HeaderBehavior.EXCEL_COLUMN_NAMES);
    } else if (existingDataMode == ExistingDataMode.REPLACE) {
      headers = headers != ExcelHeaders.HeaderBehavior.INFER ? headers :
          shouldWriteHeaders(new ExcelSheet(workbook, sheetIndex), firstRow + 1, 1, -1);

      workbook.removeSheetAt(sheetIndex);
      Sheet sheet = workbook.createSheet(sheetName);
      workbook.setSheetOrder(sheetName, sheetIndex);
      writeTableToSheet(workbook, sheet, firstRow, 1, table, rowLimit, headers != ExcelHeaders.HeaderBehavior.EXCEL_COLUMN_NAMES);
    } else if (existingDataMode == ExistingDataMode.ERROR){
      throw new ExistingDataException("Sheet already exists, and cannot be replaced in current mode.");
    } else {
      // In Append Mode, so switch to appending from the top left cell of the sheet as this is equivalent to appending to the sheet.
      ExcelRange range = new ExcelRange(sheetName, 1, 1);
      writeTableToRange(workbook, range, existingDataMode, firstRow, table, rowLimit, headers);
    }
  }

  public static void writeTableToRange(Workbook workbook, String rangeNameOrAddress, ExistingDataMode existingDataMode, int skipRows, Table table, Long rowLimit, ExcelHeaders.HeaderBehavior headers)
      throws InvalidLocationException, IllegalStateException, RangeExceededException, ExistingDataException, ColumnNameMismatchException, ColumnCountMismatchException {
    Name name = workbook.getName(rangeNameOrAddress);
    ExcelRange excelRange;
    try {
      excelRange = new ExcelRange(name == null ? rangeNameOrAddress : name.getRefersToFormula());
    } catch (IllegalArgumentException e) {
      throw new InvalidLocationException("Invalid range name or address '" + rangeNameOrAddress + "'.");
    }
    writeTableToRange(workbook, excelRange, existingDataMode, skipRows, table, rowLimit, headers);
  }

  public static void writeTableToRange(Workbook workbook, ExcelRange range, ExistingDataMode existingDataMode, int skipRows, Table table, Long rowLimit, ExcelHeaders.HeaderBehavior headers)
      throws InvalidLocationException, IllegalStateException, RangeExceededException, ExistingDataException, ColumnNameMismatchException, ColumnCountMismatchException {
    int sheetIndex = workbook.getSheetIndex(range.getSheetName());
    if (sheetIndex == -1) {
      throw new InvalidLocationException("Unknown sheet '" + range.getSheetName() + "'.");
    }
    ExcelSheet sheet = new ExcelSheet(workbook, sheetIndex);

    if (skipRows != 0) {
      if (range.isWholeColumn()) {
        range = new ExcelRange(range.getSheetName(), skipRows + 1, range.getLeftColumn(), workbook.getSpreadsheetVersion().getMaxRows(), range.getRightColumn());
      } else if (range.isSingleCell()) {
        range = new ExcelRange(range.getSheetName(), range.getTopRow() + skipRows, range.getLeftColumn());
      } else {
        range = new ExcelRange(range.getSheetName(), range.getTopRow() + skipRows, range.getLeftColumn(), range.getBottomRow(), range.getRightColumn());
      }
    }

    ExcelRange expanded = range.isSingleCell() ? ExcelRange.expandSingleCell(range, sheet) : range;
    headers = headers != ExcelHeaders.HeaderBehavior.INFER ? headers :
        shouldWriteHeaders(sheet, expanded.getTopRow(), expanded.getLeftColumn(), expanded.getRightColumn());

    if ((existingDataMode == ExistingDataMode.APPEND_BY_NAME || existingDataMode == ExistingDataMode.APPEND_BY_INDEX) &&
        rangeIsNotEmpty(workbook, expanded, sheet)) {
      appendRangeWithTable(workbook, range, existingDataMode, table, rowLimit, headers, sheet, expanded);
    } else {
      updateRangeWithTable(workbook, expanded, range.isSingleCell(), existingDataMode, table, rowLimit, headers, sheet);
    }
  }

  /**
   * Creates an empty workbook.
   * @param xls_format specifies whether the file is in Excel Binary Format (95-2003 format).
   * @return a {@link Workbook} containing the specified data.
   */
  public static Workbook createWorkbook(boolean xls_format) {
    return xls_format ? new HSSFWorkbook() : new XSSFWorkbook();
  }

  private static void appendRangeWithTable(Workbook workbook, ExcelRange range, ExistingDataMode existingDataMode, Table table, Long rowLimit, ExcelHeaders.HeaderBehavior headers, ExcelSheet sheet, ExcelRange expanded)
      throws RangeExceededException, ExistingDataException, ColumnNameMismatchException, ColumnCountMismatchException {
    Table mappedTable = switch (existingDataMode) {
      case APPEND_BY_INDEX -> ColumnMapper.mapColumnsByPosition(table, expanded.getColumnCount());
      case APPEND_BY_NAME -> {
        if (headers == ExcelHeaders.HeaderBehavior.EXCEL_COLUMN_NAMES) {
          throw new IllegalArgumentException("Cannot append by name when headers are not present in the existing data.");
        }
        String[] currentHeaders = sheet.get(expanded.getTopRow()).getCellsAsText(expanded.getLeftColumn(), expanded.getRightColumn());
        yield ColumnMapper.mapColumnsByName(table, new NameDeduplicator().makeUniqueArray(currentHeaders));
      }
      default ->
          throw new IllegalArgumentException("Internal Error: appendRangeWithTable called with illegal existing data mode '" + existingDataMode + "'.");
    };

    if (range.isSingleCell()) {
      int bottomRow = expanded.getBottomRow();
      int requiredRows = Math.min(mappedTable.rowCount(), rowLimit == null ? Integer.MAX_VALUE : rowLimit.intValue());
      expanded = new ExcelRange(expanded.getSheetName(), expanded.getLeftColumn(), bottomRow + 1, expanded.getRightColumn(), bottomRow + requiredRows);
    } else {
      int finalRow = expanded.getLastNonEmptyRow(sheet);
      if (finalRow == expanded.getBottomRow()) {
        throw new RangeExceededException("The range is already full.");
      }

      expanded = new ExcelRange(expanded.getSheetName(), expanded.getLeftColumn(), finalRow + 1, expanded.getRightColumn(), expanded.getBottomRow());
    }

    updateRangeWithTable(workbook, expanded, false, existingDataMode, mappedTable, rowLimit, ExcelHeaders.HeaderBehavior.EXCEL_COLUMN_NAMES, sheet);
  }

  private static void updateRangeWithTable(Workbook workbook, ExcelRange range, boolean singleCell, ExistingDataMode existingDataMode, Table table, Long rowLimit, ExcelHeaders.HeaderBehavior headers, ExcelSheet sheet)
      throws RangeExceededException, ExistingDataException {
    boolean writeHeaders = headers == ExcelHeaders.HeaderBehavior.USE_FIRST_ROW_AS_HEADERS;
    int requiredRows = Math.min(table.rowCount(), rowLimit == null ? Integer.MAX_VALUE : rowLimit.intValue()) + (writeHeaders ? 1 : 0);

    if (singleCell) {
      range = new ExcelRange(
          range.getSheetName(),
          range.getLeftColumn(),
          range.getTopRow(),
          Math.max(range.getRightColumn(), range.getLeftColumn() + table.getColumns().length - 1),
          Math.max(range.getBottomRow(), range.getTopRow() + requiredRows - 1));
    }

    int finalRow = range.isWholeColumn() ? workbook.getSpreadsheetVersion().getMaxRows() : range.getBottomRow();
    int availableRows = finalRow - range.getTopRow() + 1;
    if (range.getColumnCount() < table.getColumns().length || availableRows < requiredRows) {
      throw new RangeExceededException("Range is too small to fit all data.");
    }

    if (existingDataMode == ExistingDataMode.REPLACE) {
      clearRange(workbook, range, sheet);
    } else if (rangeIsNotEmpty(workbook, range, sheet)) {
      throw new ExistingDataException("Range is not empty, and cannot be replaced in current mode.");
    }

    writeTableToSheet(workbook, sheet.getSheet(), range.getTopRow() - 1, range.getLeftColumn(), table, rowLimit, writeHeaders);
  }

  /***
   * Checks if a range is empty.
   * @param workbook The workbook to check.
   * @param range The range to check.
   * @param sheet Sheet containing the range.
   * @return True if range is empty and clear is False, otherwise returns False.
   */
  private static boolean rangeIsNotEmpty(Workbook workbook, ExcelRange range, ExcelSheet sheet) {
    ExcelRange fullRange = range.getAbsoluteRange(workbook);
    for (int row = fullRange.getTopRow(); row <= fullRange.getBottomRow(); row++) {
      ExcelRow excelRow = sheet.get(row);
      if (excelRow != null && !excelRow.isEmpty(fullRange.getLeftColumn(), fullRange.getRightColumn())) {
        return true;
      }
    }
    return false;
  }

  /***
   * Clears a range of any content.
   * @param workbook The workbook to clear.
   * @param range The range to clear.
   * @param sheet Sheet containing the range.
   */
  private static void clearRange(Workbook workbook, ExcelRange range, ExcelSheet sheet) {
    ExcelRange fullRange = range.getAbsoluteRange(workbook);
    for (int row = fullRange.getTopRow(); row <= fullRange.getBottomRow(); row++) {
      ExcelRow excelRow = sheet.get(row);
      if (excelRow != null) {
        for (int column = fullRange.getLeftColumn(); column <= fullRange.getRightColumn(); column++) {
          Cell cell = excelRow.get(column);
          if (cell != null) {
            cell.setBlank();
          }
        }
      }
    }
  }


  private static void writeTableToSheet(Workbook workbook, Sheet sheet, int firstRow, int firstColumn, Table table, Long rowLimit, boolean headers)
    throws IllegalStateException {
    int rowCount = Math.min(Math.min(workbook.getSpreadsheetVersion().getMaxRows() - firstRow, rowLimit == null ? Integer.MAX_VALUE : rowLimit.intValue()), table.rowCount());
    int currentRow = firstRow;
    Column[] columns = table.getColumns();

    if (headers) {
      Row row = sheet.createRow(currentRow);
      for (int i = 0; i < columns.length; i++) {
        row.createCell(i + firstColumn - 1, CellType.STRING).setCellValue(columns[i].getName());
      }
      currentRow++;
    }

    if (rowCount == 0) {
      return;
    }

    Storage<?>[] storages = Arrays.stream(columns).map(Column::getStorage).toArray(Storage[]::new);
    for (int i = 0; i < rowCount; i++) {
      Row row = sheet.getRow(currentRow);
      if (row == null) {
        row = sheet.createRow(currentRow);
      }

      for (int j = 0; j < columns.length; j++) {
        Storage<?> storage = storages[j];
        int idx = j + firstColumn - 1;

        Cell cell = row.getCell(idx);
        if (cell == null) {
          cell = row.createCell(idx);
        }

        writeValueToCell(cell, i, storage, workbook);
      }
      currentRow++;
    }

    workbook.setForceFormulaRecalculation(true);
  }

  private static CellStyle getDateTimeStyle(Workbook workbook, String format) {
    for(int i = 0; i < workbook.getNumCellStyles(); i++) {
      CellStyle style = workbook.getCellStyleAt(i);
      if (style.getDataFormatString().equals(format)) {
        return style;
      }
    }

    CellStyle newStyle = workbook.createCellStyle();
    newStyle.setDataFormat(workbook.createDataFormat().getFormat(format));
    return newStyle;
  }

  private static void writeValueToCell(Cell cell, int j, Storage<?> storage, Workbook workbook)
    throws IllegalStateException {
    if (storage.isNa(j)) {
      cell.setBlank();
    } else if (storage instanceof DoubleStorage doubleStorage) {
      cell.setCellValue(doubleStorage.getItem(j));
    } else if (storage instanceof AbstractLongStorage longStorage) {
      cell.setCellValue(longStorage.getItem(j));
    } else if (storage instanceof BoolStorage boolStorage) {
      cell.setCellValue(boolStorage.getItem(j));
    } else {
      Object value = storage.getItemBoxed(j);
      switch (value) {
        case String s -> cell.setCellValue(s);
        case Boolean b -> cell.setCellValue(b);
        case Double d -> cell.setCellValue(d);
        case Long l -> cell.setCellValue(l);
        case LocalDateTime ldt -> {
          cell.setCellValue(ldt);
          cell.setCellStyle(getDateTimeStyle(workbook, "yyyy-MM-dd HH:mm:ss"));
        }
        case LocalDate ld -> {
          cell.setCellValue(ld);
          cell.setCellStyle(getDateTimeStyle(workbook, "yyyy-MM-dd"));
        }
        case LocalTime lt -> {
          cell.setCellValue(lt.toSecondOfDay() / SECONDS_IN_A_DAY);
          cell.setCellStyle(getDateTimeStyle(workbook, "HH:mm:ss"));
        }
        default -> {
          if (ensoToTextCallback != null) {
            cell.setCellValue(ensoToTextCallback.apply(value));
          } else {
            throw new IllegalStateException("Enso to text callback is not set. Unable to process value.");
          }
        }
      }
    }
  }

  /**
   * Determines if headers should be written for the given range in {@code INFER} mode.
   *
   * Unlike in the {@code ExcelReader}, if empty this will default to having headers.
   * @param excelSheet the Excel sheet to check.
   * @param topRow top row index (1-based) of the range to check.
   * @param startCol start column index (1-based) of the range to check.
   * @param endCol end column index (1-based) of the range to check. If -1 will continue until end of row.
   * @return EXCEL_COLUMN_NAMES if the range has headers, otherwise USE_FIRST_ROW_AS_HEADERS.
   */
  private static ExcelHeaders.HeaderBehavior shouldWriteHeaders(ExcelSheet excelSheet, int topRow, int startCol, int endCol) {
    ExcelRow row = excelSheet.get(topRow);

    // If the first row is missing or empty, should write headers.
    if (row == null || row.isEmpty(startCol, endCol)) {
      return ExcelHeaders.HeaderBehavior.USE_FIRST_ROW_AS_HEADERS;
    }

    // If the first row is not empty but not all text, should not write headers.
    if (row.getCellsAsText(startCol, endCol) == null) {
      return ExcelHeaders.HeaderBehavior.EXCEL_COLUMN_NAMES;
    }

    // If the second row is missing, empty, or not all text, should write headers.
    ExcelRow nextRow = excelSheet.get(topRow + 1);
    return (nextRow != null && nextRow.getCellsAsText(startCol, endCol) == null)
        ? ExcelHeaders.HeaderBehavior.USE_FIRST_ROW_AS_HEADERS
        : ExcelHeaders.HeaderBehavior.EXCEL_COLUMN_NAMES;
  }
}
