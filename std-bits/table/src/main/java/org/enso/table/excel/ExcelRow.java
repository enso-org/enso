package org.enso.table.excel;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Row;

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
    return isEmpty(column, column);
  }

  public boolean isEmpty(int start, int end) {
    for (int column = start; column <= end; column++) {
      if (getCellType(get(column)) != CellType._NONE) {
        return false;
      }
    }
    return true;
  }

  public int findEndRight(int start) {
    int column = start;
    while (getCellType(get(column + 1)) != CellType._NONE) {
      column++;
    }
    return column;
  }
}
