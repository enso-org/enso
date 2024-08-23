package org.enso.table.excel.xssfreader;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import javax.xml.parsers.ParserConfigurationException;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.util.XMLHelper;
import org.enso.table.excel.ExcelRow;
import org.enso.table.excel.ExcelSheet;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public class XSSFReaderSheet implements ExcelSheet {
  private final int sheetIdx;
  private final String sheetName;
  private final String relId;
  private final XSSFReaderWorkbook parent;

  private boolean readSheetData = false;
  private int firstRow;
  private int lastRow;
  private Map<Integer, SortedMap<Short, XSSFReaderSheetXMLHandler.CellValue>> rowData;

  public XSSFReaderSheet(int sheetIdx, String sheetName, String relId, XSSFReaderWorkbook parent) {
    this.sheetIdx = sheetIdx;
    this.sheetName = sheetName;
    this.relId = relId;
    this.parent = parent;
  }

  private synchronized void readSheetData() {
    if (readSheetData) {
      return;
    }

    try {
      var strings = parent.getSharedStrings();
      var styles = parent.getStyles();
      var handler =
          new XSSFReaderSheetXMLHandler(styles, strings) {
            @Override
            protected void onStartRow(int rowNum) {
              handleOnStartRow(rowNum);
            }

            @Override
            protected void onCell(int rowNumber, String ref, CellValue value) {
              handleOnCell(rowNumber, ref, value);
            }
          };

      var xmlReader = XMLHelper.newXMLReader();
      xmlReader.setContentHandler(handler);

      rowData = new HashMap<>();

      parent.withReader(
          reader -> {
            try {
              var sheet = reader.getSheet(relId);
              xmlReader.parse(new InputSource(sheet));
            } catch (SAXException | InvalidFormatException | IOException e) {
              throw new RuntimeException(e);
            }
          });

      readSheetData = true;
    } catch (SAXException | ParserConfigurationException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public int getSheetIndex() {
    return sheetIdx;
  }

  @Override
  public String getName() {
    return sheetName;
  }

  @Override
  public int getFirstRow() {
    if (!readSheetData) {
      readSheetData();
    }
    return firstRow;
  }

  @Override
  public int getLastRow() {
    if (!readSheetData) {
      readSheetData();
    }
    return lastRow;
  }

  @Override
  public ExcelRow get(int row) {
    if (!rowData.containsKey(row)) {
      return null;
    }

    return new XSSFReaderRow(row, rowData.get(row));
  }

  @Override
  public Sheet getSheet() {
    return null;
  }

  private void handleOnStartRow(int rowNum) {
    if (firstRow == 0 || rowNum < firstRow) {
      firstRow = rowNum;
    }

    if (lastRow == 0 || rowNum > lastRow) {
      lastRow = rowNum;
    }
  }

  private static boolean isColumnRef(char c) {
    return c >= 'A' && c <= 'Z';
  }

  private static short getColumn(String ref) {
    int column = 0;
    int idx = 0;
    while (idx < ref.length() && isColumnRef(ref.charAt(idx))) {
      column = column * 26 + (ref.charAt(idx) - 'A' + 1);
      idx++;
    }
    return (short) column;
  }

  private void handleOnCell(int rowNumber, String ref, XSSFReaderSheetXMLHandler.CellValue value) {
    short column = getColumn(ref);

    if (!rowData.containsKey(rowNumber)) {
      rowData.put(rowNumber, new TreeMap<>());
    }
    rowData.get(rowNumber).put(column, value);
  }
}
