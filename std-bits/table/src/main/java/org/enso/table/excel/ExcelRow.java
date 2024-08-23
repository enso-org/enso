package org.enso.table.excel;

import java.time.LocalDateTime;
import java.time.ZoneId;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.graalvm.polyglot.Context;

/** Wrapper class to handle Excel rows. */
public interface ExcelRow {
  /** Gets the index of the row within the sheet (1-based). */
  int getRowIndex();

  /** Gets the initial column index within the row (1-based). */
  int getFirstColumn();

  /** Gets the final column index within the row (1-based). */
  int getLastColumn();

  /** Gets the cell at the given index within the row (1-based). */
  Object getCellValue(int column);

  /** Gets the text of a cell at the given index within the row (1-based). */
  String getCellText(int column);

  /** Gets the cell at the given index within the row (1-based). */
  Cell get(int column);

  /** Checks if the specified cell is empty. */
  boolean isEmpty(int column);

  /** Checks if the specified set of cells are empty. */
  boolean isEmpty(int start, int end);

  /** Gets the cells as text. */
  String[] getCellsAsText(int startCol, int endCol);

  /** Gets the underlying Apache POI Sheet object. */
  static ExcelRow fromSheet(Sheet sheet, int rowIndex) {
    var row = sheet.getRow(rowIndex - 1);
    return row == null
        ? null
        : new ExcelRowFromSheet(row, row.getFirstCellNum() + 1, row.getLastCellNum());
  }

  static boolean isEmptyHelper(ExcelRow row, int start, int end) {
    Context context = Context.getCurrent();
    int currentEnd = end == -1 ? row.getLastColumn() : end;
    for (int column = Math.max(row.getFirstColumn(), start);
        column <= Math.min(row.getLastColumn(), currentEnd);
        column++) {
      if (!row.isEmpty(column)) {
        return false;
      }

      context.safepoint();
    }
    return true;
  }

  record ExcelRowFromSheet(Row row, int firstColumn, int lastColumn) implements ExcelRow {
    private static final DataFormatter formatter = new DataFormatter();

    public int getRowIndex() {
      return row.getRowNum() + 1;
    }

    public int getFirstColumn() {
      return firstColumn;
    }

    public int getLastColumn() {
      return lastColumn;
    }

    public Cell get(int column) {
      return (column < firstColumn || column > lastColumn) ? null : row.getCell(column - 1);
    }

    public Object getCellValue(int column) {
      Cell cell = get(column);
      CellType cellType = getCellType(cell);
      switch (cellType) {
        case NUMERIC:
          double dblValue = cell.getNumericCellValue();
          if (DateUtil.isCellDateFormatted(cell)) {
            var dateTime = DateUtil.getLocalDateTime(dblValue);
            if (dateTime.isBefore(LocalDateTime.of(1900, 1, 2, 0, 0))) {
              // Excel stores times as if they are on the 1st January 1900.
              // Due to the 1900 leap year bug might be 31st December 1899.
              return dateTime.toLocalTime();
            }
            if (dateTime.getHour() == 0 && dateTime.getMinute() == 0 && dateTime.getSecond() == 0) {
              var dateFormat = cell.getCellStyle().getDataFormatString();
              if (!dateFormat.contains("h") && !dateFormat.contains("H")) {
                return dateTime.toLocalDate();
              }
            }
            return dateTime.atZone(ZoneId.systemDefault());
          } else {
            if (dblValue == (long) dblValue) {
              return (long) dblValue;
            } else {
              return dblValue;
            }
          }
        case STRING:
          return cell.getStringCellValue();
        case BOOLEAN:
          return cell.getBooleanCellValue();
        default:
          return null;
      }
    }

    public String getCellText(int column) {
      Cell cell = get(column);
      return cell == null ? "" : formatter.formatCellValue(cell);
    }

    public boolean isEmpty(int column) {
      CellType cellType = getCellType(get(column));
      return (cellType == CellType._NONE) || (cellType == CellType.BLANK);
    }

    public boolean isEmpty(int start, int end) {
      return isEmptyHelper(this, start, end);
    }

    public String[] getCellsAsText(int startCol, int endCol) {
      Context context = Context.getCurrent();
      int currentEndCol = endCol == -1 ? getLastColumn() : endCol;

      String[] output = new String[currentEndCol - startCol + 1];
      for (int col = startCol; col <= currentEndCol; col++) {
        Cell cell = get(col);
        CellType type = getCellType(cell);
        if (type != CellType._NONE && type != CellType.BLANK && type != CellType.STRING) {
          return null;
        }
        output[col - startCol] =
            type == CellType.STRING && cell != null ? cell.getStringCellValue() : "";
        context.safepoint();
      }

      return output;
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
  }
}
