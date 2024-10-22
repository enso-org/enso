package org.enso.table.excel;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.ExcelNumberFormat;
import org.apache.poi.ss.usermodel.FormulaError;
import org.apache.poi.ss.usermodel.Row;
import org.graalvm.polyglot.Context;

/** Wrapper class to handle Excel rows. */
public class ExcelRow {
  private static final DataFormatter formatter = new DataFormatter();

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
        var nf = ExcelNumberFormat.from(cell, null);
        if (nf != null && DateUtil.isADateFormat(nf.getIdx(), nf.getFormat())) {
          var temporal = ExcelUtils.fromExcelDateTime(dblValue);
          if (temporal == null) {
            return null;
          }
          return switch (temporal) {
            case LocalDate date -> {
              var dateFormat = cell.getCellStyle().getDataFormatString();
              yield (!dateFormat.contains("h") && !dateFormat.contains("H"))
                  ? date.atStartOfDay(ZoneId.systemDefault())
                  : date;
            }
            case LocalDateTime dateTime -> dateTime.atZone(ZoneId.systemDefault());
            default -> temporal;
          };
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
    Context context = Context.getCurrent();
    int currentEnd = end == -1 ? getLastColumn() : end;
    for (int column = Math.max(getFirstColumn(), start);
        column <= Math.min(getLastColumn(), currentEnd);
        column++) {
      if (!isEmpty(column)) {
        return false;
      }

      context.safepoint();
    }
    return true;
  }

  public int findEndRight(int start) {
    Context context = Context.getCurrent();
    int column = start;
    while (!isEmpty(column + 1)) {
      column++;
      context.safepoint();
    }
    return column;
  }

  /** Returns the formatted cell value. */
  public String getFormattedCell(int col) {
    var cell = get(col);
    if (cell == null) {
      return "";
    }

    var rawCellType = cell.getCellType();
    var cellType =
        rawCellType == CellType.FORMULA ? cell.getCachedFormulaResultType() : rawCellType;

    return switch (cellType) {
      case ERROR ->
      // Want to show the error message rather than empty.
      FormulaError.forInt(cell.getErrorCellValue()).getString();
      case NUMERIC -> {
        // Special handling for Number or Date cells as want to keep formatting.
        var format = ExcelNumberFormat.from(cell, null);
        var value = cell.getNumericCellValue();
        yield format == null
            ? Double.toString(value)
            : formatter.formatRawCellContents(value, format.getIdx(), format.getFormat());
      }
      default -> {
        // Use the default read and then toString.
        var value = getCellValue(col);
        yield value == null ? "" : value.toString();
      }
    };
  }

  public String[] getCellsAsText(int startCol, int endCol) {
    Context context = Context.getCurrent();
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
      context.safepoint();
    }

    return output;
  }
}
