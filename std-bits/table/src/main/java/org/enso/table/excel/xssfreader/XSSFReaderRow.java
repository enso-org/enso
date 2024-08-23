package org.enso.table.excel.xssfreader;

import java.util.SortedMap;
import org.apache.poi.ss.usermodel.Cell;
import org.enso.table.excel.ExcelRow;

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
    return new String[0];
  }
}
