package org.enso.table.excel.xssfreader;

import java.time.LocalDateTime;
import java.util.SortedMap;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.enso.table.excel.ExcelRow;

public class XSSFReaderRow implements ExcelRow {
  private static final DataFormatter formatter = new DataFormatter();
  private final SortedMap<Short, XSSFReaderSheetXMLHandler.CellValue> data;
  private final boolean use1904Dates;

  public XSSFReaderRow(
      SortedMap<Short, XSSFReaderSheetXMLHandler.CellValue> data, boolean use1904Dates) {
    this.data = data;
    this.use1904Dates = use1904Dates;
  }

  @Override
  public int getFirstColumn() {
    return data.firstKey();
  }

  @Override
  public int getLastColumn() {
    return data.lastKey();
  }

  @Override
  public Cell get(int column) {
    // Not supported as we don't have the underlying Apache POI Cell object.
    throw new UnsupportedOperationException("XSSFReader does not support getting the Cell object.");
  }

  @Override
  public Object getCellValue(int column) {
    var cell = data.get((short) column);
    if (cell == null) {
      return null;
    }

    var dataType = cell.dataType();
    return switch (dataType) {
      case BLANK -> null;
      case BOOL -> cell.getBooleanValue();
      case DATE -> LocalDateTime.parse(cell.strValue()); // Don't believe used by Excel.
      case INLINE_STRING, SST_STRING, FORMULA_STRING -> cell.strValue();
      case INTEGER -> cell.getIntegerValue();
      case NUMBER -> {
        double dbl = cell.getNumberValue();
        long longVal = (long) dbl;
        if (dbl == longVal) {
          yield (long) dbl;
        } else {
          yield dbl;
        }
      }
      case OLE_DATE -> cell.getDateValue(use1904Dates);
      case OLE_DATETIME -> cell.getDateTimeValue(use1904Dates);
      case ERROR -> null;
    };
  }

  @Override
  public String getCellText(int column) {
    var cell = data.get((short) column);
    if (cell == null) {
      return "";
    }

    var dataType = cell.dataType();
    return switch (dataType) {
      case BLANK -> "";
      case NUMBER, OLE_DATETIME, OLE_DATE, INTEGER -> {
        // Special handling for Number or Date cells as want to keep formatting.
        var formatText = cell.format();
        if (formatText == null || formatText.isEmpty()) {
          yield cell.strValue();
        }
        yield formatter.formatRawCellContents(cell.getNumberValue(), -1, formatText, use1904Dates);
      }
      case BOOL -> cell.getBooleanValue() ? "TRUE" : "FALSE";
      default -> cell.strValue();
    };
  }

  @Override
  public boolean isEmpty(int column) {
    var cell = data.get((short) column);
    return cell == null || cell.strValue().isEmpty();
  }

  @Override
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

  @Override
  public String[] getCellsAsText(int startCol, int endCol) {
    int currentEndCol = endCol == -1 ? getLastColumn() : endCol;

    String[] output = new String[currentEndCol - startCol + 1];
    for (int col = startCol; col <= currentEndCol; col++) {

      var cell = data.get((short) col);
      if (cell != null && !cell.dataType().isString()) {
        // Short circuit if find not a string cell.
        return null;
      }

      output[col - startCol] = cell == null ? "" : cell.strValue();
    }

    return output;
  }
}
