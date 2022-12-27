package org.enso.table.excel;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Row;

import java.time.LocalDateTime;
import java.time.ZoneId;

/** Wrapper class to handle Excel rows. */
public class ExcelRow {
  private final Row row;
  private final int firstColumn;
  private final int lastColumn;

  public ExcelRow(Row row) {
    this.row = row;
    this.firstColumn = row.getFirstCellNum() + 1;
    this.lastColumn = row.getLastCellNum();
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
        if (DateUtil.isCellDateFormatted(cell) && DateUtil.isValidExcelDate(dblValue)) {
          var dateTime = DateUtil.getLocalDateTime(dblValue);
          if (dateTime.isBefore(LocalDateTime.of(1900, 1, 2, 0, 0))) {
            // Excel stores times as if they are on the 1st January 1900.
            // Due to the 1900 leap year bug might be 31st December 1899.
            return dateTime.toLocalTime();
          }
          if (dateTime.getHour() == 0 && dateTime.getMinute() == 0 && dateTime.getSecond() == 0) {
            return dateTime.toLocalDate();
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

  public static CellType getCellType(Cell cell) {
    if (cell == null) {
      return CellType._NONE;
    }

    CellType cellType = cell.getCellType();
    if (cellType == CellType.FORMULA) {
      cellType = cell.getCachedFormulaResultType();
    }

    return cellType;
  }

  public boolean isEmpty(int column) {
    CellType cellType = getCellType(get(column));
    return (cellType == CellType._NONE) || (cellType == CellType.BLANK);
  }

  public boolean isEmpty(int start, int end) {
    int currentEnd = end == -1 ? getLastColumn() : end;
    for (int column = Math.max(getFirstColumn(), start);
        column <= Math.min(getLastColumn(), currentEnd);
        column++) {
      if (!isEmpty(column)) {
        return false;
      }
    }
    return true;
  }

  public int findEndRight(int start) {
    int column = start;
    while (!isEmpty(column + 1)) {
      column++;
    }
    return column;
  }

  public String[] getCellsAsText(int startCol, int endCol) {
    int currentEndCol = endCol == -1 ? getLastColumn() : endCol;

    String[] output = new String[currentEndCol - startCol + 1];
    for (int col = startCol; col <= currentEndCol; col++) {
      Cell cell = get(col);
      CellType type = ExcelRow.getCellType(cell);
      if (type != CellType._NONE && type != CellType.BLANK && type != CellType.STRING) {
        return null;
      }
      output[col - startCol] =
          type == CellType.STRING && cell != null ? cell.getStringCellValue() : "";
    }

    return output;
  }
}
