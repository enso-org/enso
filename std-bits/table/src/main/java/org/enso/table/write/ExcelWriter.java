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

  public static void writeTableToSheet(Workbook workbook, int sheetIndex, boolean replace, int firstRow, Table table, int rowLimit, boolean headers) {
    if (sheetIndex == 0 || sheetIndex > workbook.getNumberOfSheets()) {
      int i = 1;
      while (workbook.getSheet("Sheet" + i) != null) {
        i++;
      }

      writeTableToSheet(workbook, workbook.createSheet("Sheet" + i), firstRow, table, rowLimit, headers);
    } else if (replace) {
      String sheetName = workbook.getSheetName(sheetIndex - 1);
      workbook.removeSheetAt(sheetIndex - 1);

      Sheet sheet = workbook.createSheet(sheetName);
      workbook.setSheetOrder(sheetName, sheetIndex - 1);
      writeTableToSheet(workbook, sheet, firstRow, table, rowLimit, headers);
    } else {
      throw new IllegalArgumentException("Sheet already exists, and cannot be replaced in current mode.");
    }
  }

  public static void writeTableToSheet(Workbook workbook, String sheetName, boolean replace, int firstRow, Table table, int rowLimit, boolean headers) {
    int sheetIndex = workbook.getSheetIndex(sheetName);
    if (sheetIndex == -1) {
      writeTableToSheet(workbook, workbook.createSheet(sheetName), firstRow, table, rowLimit, headers);
    } else if (replace) {
      workbook.removeSheetAt(sheetIndex);
      Sheet sheet = workbook.createSheet(sheetName);
      workbook.setSheetOrder(sheetName, sheetIndex);
      writeTableToSheet(workbook, sheet, firstRow, table, rowLimit, headers);
    } else {
      throw new IllegalArgumentException("Sheet '" + sheetName + "' already exists, and cannot be replaced in current mode.");
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

  private static void writeTableToSheet(Workbook workbook, Sheet sheet, int firstRow, Table table, int rowLimit, boolean headers) {
    int currentRow = firstRow;
    Column[] columns = table.getColumns();

    if (headers) {
      Row row = sheet.createRow(currentRow);
      for (int i = 0; i < columns.length; i++) {
        row.createCell(i, CellType.STRING).setCellValue(columns[i].getName());
      }
      currentRow++;
    }

    if (rowLimit == 0 || table.rowCount() == 0) {
      return;
    }

    Storage[] storages = Arrays.stream(columns).map(Column::getStorage).toArray(Storage[]::new);
    for (int i = 0; i < rowLimit; i++) {
      Row row = sheet.createRow(currentRow);
      for (int j = 0; j < columns.length; j++) {
        Storage storage = storages[i];
        writeValueToCell(row.createCell(j), j, storage, workbook);
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

  public static void writeValueToCell(Cell cell, int j, Storage storage, Workbook workbook) {
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
}
