package org.enso.table.write;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.DoubleStorage;
import org.enso.table.data.column.storage.LongStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.error.ExistingDataException;
import org.enso.table.error.RangeExceededException;
import org.enso.table.excel.ExcelHeaders;
import org.enso.table.excel.ExcelRange;
import org.enso.table.excel.ExcelRow;
import org.enso.table.excel.ExcelSheet;

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

  public static void getEnsoToTextCallback(Function<Object, Boolean> callback) {
    ensoToTextCallback = callback;
  }

  public static void writeTableToSheet(Workbook workbook, int sheetIndex, boolean replace, int firstRow, Table table, Long rowLimit, ExcelHeaders.HeaderBehavior headers)
      throws ExistingDataException {
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
    } else if (replace) {
      if (headers == ExcelHeaders.HeaderBehavior.INFER) {
        ExcelSheet excelSheet = new ExcelSheet(workbook, sheetIndex);
        headers = hasHeaders(excelSheet, firstRow + 1, 1, -1)
            ? ExcelHeaders.HeaderBehavior.USE_FIRST_ROW_AS_HEADERS
            : ExcelHeaders.HeaderBehavior.EXCEL_COLUMN_NAMES;
      }

      String sheetName = workbook.getSheetName(sheetIndex - 1);
      workbook.removeSheetAt(sheetIndex - 1);

      Sheet sheet = workbook.createSheet(sheetName);
      workbook.setSheetOrder(sheetName, sheetIndex - 1);
      writeTableToSheet(workbook, sheet, firstRow, 1, table, rowLimit, headers != ExcelHeaders.HeaderBehavior.EXCEL_COLUMN_NAMES);
    } else {
      throw new ExistingDataException("Sheet already exists, and cannot be replaced in current mode.");
    }
  }

  public static void writeTableToSheet(Workbook workbook, String sheetName, boolean replace, int firstRow, Table table, Long rowLimit, ExcelHeaders.HeaderBehavior headers)
      throws ExistingDataException {
    int sheetIndex = workbook.getSheetIndex(sheetName);
    if (sheetIndex == -1) {
      writeTableToSheet(workbook, workbook.createSheet(sheetName), firstRow, 1, table, rowLimit, headers != ExcelHeaders.HeaderBehavior.EXCEL_COLUMN_NAMES);
    } else if (replace) {
      if (headers == ExcelHeaders.HeaderBehavior.INFER) {
        ExcelSheet excelSheet = new ExcelSheet(workbook, sheetIndex);
        headers = hasHeaders(excelSheet, firstRow + 1, 1, -1)
            ? ExcelHeaders.HeaderBehavior.USE_FIRST_ROW_AS_HEADERS
            : ExcelHeaders.HeaderBehavior.EXCEL_COLUMN_NAMES;
    }

      workbook.removeSheetAt(sheetIndex);
      Sheet sheet = workbook.createSheet(sheetName);
      workbook.setSheetOrder(sheetName, sheetIndex);
      writeTableToSheet(workbook, sheet, firstRow, 1, table, rowLimit, headers != ExcelHeaders.HeaderBehavior.EXCEL_COLUMN_NAMES);
    } else {
      throw new ExistingDataException("Sheet '" + sheetName + "' already exists, and cannot be replaced in current mode.");
    }
  }

  public static void writeTableToRange(Workbook workbook, String rangeNameOrAddress, boolean replace, int skipRows, Table table, Long rowLimit, ExcelHeaders.HeaderBehavior headers)
      throws IllegalArgumentException, RangeExceededException, ExistingDataException {
    Name name = workbook.getName(rangeNameOrAddress);
    ExcelRange excelRange =
        new ExcelRange(name == null ? rangeNameOrAddress : name.getRefersToFormula());
    writeTableToRange(workbook, excelRange, replace, skipRows, table, rowLimit, headers);
  }

  public static void writeTableToRange(Workbook workbook, ExcelRange range, boolean replace, int skipRows, Table table, Long rowLimit, ExcelHeaders.HeaderBehavior headers)
      throws IllegalArgumentException, RangeExceededException, ExistingDataException {
    int sheetIndex = workbook.getSheetIndex(range.getSheetName());
    if (sheetIndex == -1) {
      throw new IllegalArgumentException("Unknown sheet '" + range.getSheetName() + "'.");
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

    if (range.isSingleCell()) {
      ExcelRange expanded = ExcelRange.expandSingleCell(range, sheet);

      if (headers == ExcelHeaders.HeaderBehavior.INFER) {
        headers = hasHeaders(sheet, expanded.getTopRow(), expanded.getLeftColumn(), expanded.getRightColumn())
            ? ExcelHeaders.HeaderBehavior.USE_FIRST_ROW_AS_HEADERS
            : ExcelHeaders.HeaderBehavior.EXCEL_COLUMN_NAMES;
      }

      // Expand to cover required size
      int rowCount = (headers == ExcelHeaders.HeaderBehavior.USE_FIRST_ROW_AS_HEADERS ? 1 : 0) + table.rowCount();
      if (expanded.getColumnCount() < table.getColumns().length || expanded.getRowCount() < rowCount) {
        expanded = new ExcelRange(
            expanded.getSheetName(),
            expanded.getLeftColumn(),
            expanded.getTopRow(),
            Math.max(expanded.getRightColumn(), expanded.getLeftColumn() + table.getColumns().length - 1),
            Math.max(expanded.getBottomRow(), expanded.getTopRow() + rowCount - 1));
      }

      checkExistingRange(workbook, expanded, replace, sheet);
    } else {
      // Check Size of Range
      int rowCount = Math.min(Math.min(workbook.getSpreadsheetVersion().getMaxRows() - range.getTopRow() + 1, rowLimit == null ? Integer.MAX_VALUE : rowLimit.intValue()), table.rowCount());
      if (range.getColumnCount() < table.getColumns().length || range.getRowCount() < rowCount) {
        throw new RangeExceededException("Range is too small to fit all columns.");
      }

      if (headers == ExcelHeaders.HeaderBehavior.INFER) {
        headers = hasHeaders(sheet, range.getTopRow(), range.getLeftColumn(), range.isWholeRow() ? -1 : range.getRightColumn())
            ? ExcelHeaders.HeaderBehavior.USE_FIRST_ROW_AS_HEADERS
            : ExcelHeaders.HeaderBehavior.EXCEL_COLUMN_NAMES;
      }

      checkExistingRange(workbook, range, replace, sheet);
    }

    writeTableToSheet(workbook, sheet.getSheet(), range.getTopRow() - 1, range.getLeftColumn(), table, rowLimit, headers != ExcelHeaders.HeaderBehavior.EXCEL_COLUMN_NAMES);
  }

  private static void checkExistingRange(Workbook workbook, ExcelRange range, boolean replace, ExcelSheet sheet) throws ExistingDataException {
    int topRow = range.isWholeColumn() ? 1 : range.getTopRow();
    int bottomRow = range.isWholeColumn() ? workbook.getSpreadsheetVersion().getMaxRows() : range.getBottomRow();
    int leftColumn = range.isWholeRow() ? 1 : range.getLeftColumn();
    int rightColumn = range.isWholeRow() ? workbook.getSpreadsheetVersion().getMaxColumns() : range.getRightColumn();

    for (int row = topRow; row <= bottomRow; row++) {
      ExcelRow excelRow = sheet.get(row);
      if (excelRow != null) {
        for (int column = leftColumn; column <= rightColumn; column++) {
          Cell cell = excelRow.get(column);
          if (cell != null) {
            if (replace) {
              cell.setBlank();
            } else {
              throw new ExistingDataException("Range is not empty, and cannot be replaced in current mode.");
            }
          }
        }
      }
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

  private static void writeTableToSheet(Workbook workbook, Sheet sheet, int firstRow, int firstColumn, Table table, Long rowLimit, boolean headers) {
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

    Storage[] storages = Arrays.stream(columns).map(Column::getStorage).toArray(Storage[]::new);
    for (int i = 0; i < rowCount; i++) {
      Row row = sheet.getRow(currentRow);
      if (row == null) {
        row = sheet.createRow(currentRow);
      }

      for (int j = 0; j < columns.length; j++) {
        Storage storage = storages[j];

        Cell cell = row.getCell(j + firstColumn - 1);
        if (cell == null) {
          cell = row.createCell(j + firstColumn - 1);
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

  private static void writeValueToCell(Cell cell, int j, Storage storage, Workbook workbook) {
    if (storage.isNa(j)) {
      cell.setBlank();
    } else if (storage instanceof DoubleStorage doubleStorage) {
      cell.setCellValue(doubleStorage.getItem(j));
    } else if (storage instanceof LongStorage longStorage) {
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
            throw new IllegalArgumentException("Enso to text callback is not set. Unable to process value.");
          }
        }
      }
    }
  }

  private static boolean hasHeaders(ExcelSheet excelSheet, int topRow, int startCol, int endCol) {
    ExcelRow row = excelSheet.get(topRow);

    // If the first row is missing or empty, return true as defaults to writing headers.
    if (row == null || row.isEmpty(startCol, endCol)) {
      return true;
    }

    // If the first row is not empty, check if all text.
    if (row.getCellsAsText(startCol, endCol) == null) {
      return false;
    }

    // If the second row is missing or empty or contains text, return false.
    ExcelRow nextRow = excelSheet.get(topRow + 1);
    return (nextRow != null && nextRow.getCellsAsText(startCol, endCol) == null);
  }
}
