package org.enso.table.excel.xssfreader;

import java.util.HashMap;
import java.util.Map;
import org.apache.poi.xssf.model.StylesTable;

/** Provides the format strings for number formats in an XSSF workbook. */
public class XSSFReaderFormats {
  private final StylesTable stylesTable;
  private final Map<Short, String> numberFormats = new HashMap<>();

  public XSSFReaderFormats(StylesTable stylesTable) {
    this.stylesTable = stylesTable;
  }

  public String getNumberFormatAt(short styleIdx) {
    if (numberFormats.containsKey(styleIdx)) {
      return numberFormats.get(styleIdx);
    }

    var style = stylesTable.getStyleAt(styleIdx);
    var format = style == null ? "General" : style.getDataFormatString();
    if (format == null || format.equals("General")) {
      format = "";
    }
    numberFormats.put(styleIdx, format);
    return format;
  }
}
