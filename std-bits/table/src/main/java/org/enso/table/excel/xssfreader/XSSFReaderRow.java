package org.enso.table.excel.xssfreader;

import java.util.SortedMap;
import org.apache.poi.ss.usermodel.Cell;
import org.enso.table.excel.ExcelRow;
import org.graalvm.polyglot.Context;

public class XSSFReaderRow implements ExcelRow {
  private final int index;
  private final SortedMap<Short, XSSFReaderSheetXMLHandler.CellValue> data;

  public XSSFReaderRow(int index, SortedMap<Short, XSSFReaderSheetXMLHandler.CellValue> data) {
    this.index = index;
    this.data = data;
  }

  @Override
  public int getRowIndex() {
    return index;
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
  public Object getCellValue(int column) {
    var cell = data.get((short) column);
    return (cell == null ? null : cell.getValue(true));
  }

  @Override
  public String getCellText(int column) {
    var cell = data.get((short) column);
    return cell == null ? "" : cell.strValue();
  }

  @Override
  public Cell get(int column) {
    return null;
  }

  @Override
  public boolean isEmpty(int column) {
    var cell = data.get((short) column);
    return cell == null || cell.strValue().isEmpty();
  }

  @Override
  public boolean isEmpty(int start, int end) {
    return ExcelRow.isEmptyHelper(this, start, end);
  }

  @Override
  public String[] getCellsAsText(int startCol, int endCol) {
    Context context = Context.getCurrent();
    int currentEndCol = endCol == -1 ? getLastColumn() : endCol;

    String[] output = new String[currentEndCol - startCol + 1];
    for (int col = startCol; col <= currentEndCol; col++) {

      var cell = data.get((short) col);
      if (cell != null) {
        var dataType = cell.dataType();
        if (dataType != XSSFReaderSheetXMLHandler.XSSDataType.INLINE_STRING
            && dataType != XSSFReaderSheetXMLHandler.XSSDataType.SST_STRING) {
          return null;
        }
      }

      output[col - startCol] = cell == null ? "" : cell.strValue();
      context.safepoint();
    }

    return output;
  }
}
