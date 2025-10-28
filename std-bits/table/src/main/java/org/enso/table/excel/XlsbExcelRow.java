package org.enso.table.excel;

import java.util.List;
import org.apache.poi.ss.usermodel.Cell;
import org.enso.table.excel.XlsbExcelWorkbookReader.XlsbSheetContentsHandler.RowData;
import org.graalvm.polyglot.Context;

/** Simple row wrapper backed by the parsed XLSB cell values. */
public final class XlsbExcelRow implements ExcelRow {

  private final List<Object> values;
  private final int firstColumn;
  private final int lastColumn;

  public XlsbExcelRow(RowData rowData) {
    this.values = rowData.values();
    this.firstColumn = rowData.firstColumn();
    this.lastColumn = rowData.lastColumn();
  }

  @Override
  public int getFirstColumn() {
    return firstColumn;
  }

  @Override
  public int getLastColumn() {
    return lastColumn;
  }

  @Override
  public Object getCellValue(int column) {
    if (!isWithinBounds(column)) {
      return null;
    }
    return values.get(column - 1);
  }

  @Override
  public String getCellText(int column) {
    if (!isWithinBounds(column)) {
      return "";
    }

    var value = values.get(column - 1);
    return value == null ? "" : value.toString();
  }

  @Override
  public Cell get(int column) {
    return null;
  }

  @Override
  public boolean isEmpty(int column) {
    return getCellText(column).isEmpty();
  }

  @Override
  public boolean isEmpty(int start, int end) {
    return ExcelRow.isEmptyHelper(this, start, end);
  }

  @Override
  public String[] getCellsAsText(int startCol, int endCol) {
    int effectiveLastColumn = endCol == -1 ? lastColumn : endCol;
    if (startCol < 1 || effectiveLastColumn < startCol) {
      return null;
    }

    String[] output = new String[effectiveLastColumn - startCol + 1];
    var context = Context.getCurrent();
    for (int column = startCol; column <= effectiveLastColumn; column++) {
      context.safepoint();

      if (!isWithinBounds(column)) {
        output[column - startCol] = "";
      } else {
        var value = values.get(column - 1);
        if (value == null) {
          output[column - startCol] = "";
        } else if (value instanceof CharSequence textValue) {
          output[column - startCol] = textValue.toString();
        } else {
          return null;
        }
      }
    }

    return output;
  }

  private boolean isWithinBounds(int column) {
    return values != null && column >= 1 && column <= values.size();
  }
}
