package org.enso.table.excel.xssfreader;

import static org.apache.poi.xssf.usermodel.XSSFRelation.NS_SPREADSHEETML;

import java.time.ZonedDateTime;
import java.time.temporal.Temporal;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.xssf.model.SharedStrings;
import org.apache.poi.xssf.usermodel.XSSFRichTextString;
import org.enso.table.excel.ExcelUtils;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

/** Based on the XSSFSheetXMLHandler class from Apache POI. */
/**
 * SAX-based Handler to Read Excel XML on top of POI support. Technical specification can be found
 * at:
 * https://learn.microsoft.com/en-us/openspecs/office_standards/ms-oe376/db9b9b72-b10b-4e7e-844c-09f88c972219
 * https://ecma-international.org/publications-and-standards/standards/ecma-376/
 */
public class XSSFReaderSheetXMLHandler extends DefaultHandler {
  private final XSSFReaderFormats styles;
  private final SharedStrings sharedStrings;

  public enum XSSDataType {
    BLANK,
    BOOL,
    DATE,
    ERROR,
    INLINE_STRING,
    SST_STRING,
    NUMBER,
    INTEGER,
    OLE_DATE,
    OLE_DATETIME,
    FORMULA_STRING;

    public boolean isString() {
      return this == INLINE_STRING || this == SST_STRING || this == FORMULA_STRING;
    }
  }

  // Record if seen a value element
  private boolean seenValue;

  // Set when V start element is seen
  private boolean vIsOpen;

  // Set when an Inline String "is" is seen
  private boolean isIsOpen;

  // The current row being read (or -1 if not in a row)
  private int rowNumber = -1;

  // Handle missing rowNumber in the XML (happens in Excel), first row would be row 1.
  private int nextRowNumber = 1;

  // The current cell being read (or null if not in a cell)
  private String cellRef;

  // Set when cell start element is seen, used when cell close element is seen.
  private XSSDataType dataType;

  // Gathers characters as they are seen.
  private final StringBuilder value = new StringBuilder(64);
  private String numberFormat = null;

  public XSSFReaderSheetXMLHandler(XSSFReaderFormats styles, SharedStrings strings) {
    this.styles = styles;
    this.sharedStrings = strings;
  }

  private boolean isTextTag(String name) {
    return "v".equals(name) || "inlineStr".equals(name) || ("t".equals(name) && isIsOpen);
  }

  @Override
  public void startElement(String uri, String localName, String qName, Attributes attributes) {
    if (uri != null && !NS_SPREADSHEETML.equals(uri)) {
      return;
    }

    if (isTextTag(localName)) {
      seenValue = true;
      vIsOpen = true;
      if (!isIsOpen) {
        value.setLength(0);
      }
    } else {
      switch (localName) {
        case "dimension": // Dimensions of sheet
          var dimension = attributes.getValue("ref");
          if (dimension != null) {
            onDimensions(dimension);
          }
          break;
        case "row": // Row
          String rowNumStr = attributes.getValue("r");
          rowNumber = rowNumStr == null ? nextRowNumber : Integer.parseInt(rowNumStr);
          onStartRow(rowNumber);
          break;
        case "c": // Cell
          cellRef = attributes.getValue("r");
          seenValue = false;

          String cellType = attributes.getValue("t");
          if (cellType == null) {
            cellType = "n"; // Number is default
          }

          dataType =
              switch (cellType) {
                case "b" -> XSSDataType.BOOL;
                case "e" -> XSSDataType.ERROR;
                case "d" -> XSSDataType.DATE; // Date in ISO 8601 format.
                case "inlineStr" -> XSSDataType.INLINE_STRING;
                case "s" -> XSSDataType.SST_STRING;
                case "str" -> XSSDataType.FORMULA_STRING; // String formula
                default -> XSSDataType.NUMBER;
              };

          // Read the format for NUMBER
          numberFormat = null;
          if (dataType == XSSDataType.NUMBER) {
            String cellStyleStr = attributes.getValue("s");
            if (cellStyleStr != null) {
              short styleIndex = (short) Integer.parseInt(cellStyleStr);
              numberFormat = styles.getNumberFormatAt(styleIndex);
            }
          }
          break;
        case "is": // Inline String
          isIsOpen = true;
          value.setLength(0);
          break;
      }
    }
  }

  /** Captures characters if a suitable element is open. */
  @Override
  public void characters(char[] ch, int start, int length) {
    if (vIsOpen) {
      value.append(ch, start, length);
    }
  }

  @Override
  public void endElement(String uri, String localName, String qName) {
    if (uri != null && !NS_SPREADSHEETML.equals(uri)) {
      return;
    }

    if (isTextTag(localName)) {
      vIsOpen = false;
    } else {
      switch (localName) {
        case "sheetData" -> onSheetEnd();
        case "row" -> {
          nextRowNumber = rowNumber + 1;
          rowNumber = -1;
        }
        case "c" -> outputCellValue();
        case "is" -> isIsOpen = false;
        case "v" -> vIsOpen = false;
      }
    }
  }

  public record CellValue(XSSDataType dataType, String strValue, String format) {
    public boolean getBooleanValue() {
      return strValue.charAt(0) == '1';
    }

    public double getNumberValue() {
      return Double.parseDouble(strValue);
    }

    public long getIntegerValue() {
      return Long.parseLong(strValue);
    }

    public Temporal getDateValue(boolean use1904Dates) {
      return use1904Dates
          ? ExcelUtils.fromExcelDateTime1904(getIntegerValue())
          : ExcelUtils.fromExcelDateTime(getIntegerValue());
    }

    public Temporal getDateTimeValue(boolean use1904Dates) {
      if (use1904Dates) {
        var datetime = ExcelUtils.fromExcelDateTime1904(getNumberValue());
        if (datetime instanceof ZonedDateTime zdt
            && zdt.getYear() == 1904
            && zdt.getDayOfYear() == 1
            && !format.contains("y")
            && !format.contains("M")
            && !format.contains("d")) {
          datetime = zdt.toLocalTime();
        }
        return datetime;
      }

      return ExcelUtils.fromExcelDateTime(getNumberValue());
    }
  }

  public String getStringValue() {
    if (dataType == XSSDataType.SST_STRING) {
      return getSharedString(value.toString());
    } else if (dataType == XSSDataType.INLINE_STRING) {
      return new XSSFRichTextString(value.toString()).toString();
    }
    return value.toString();
  }

  private String getSharedString(String value) {
    int idx = Integer.parseInt(value);
    var ss = sharedStrings.getItemAt(idx);
    return ss == null ? null : ss.toString();
  }

  private void outputCellValue() {
    short columnNumber = 0;
    int i = 0;
    char c;
    while (i < cellRef.length() && (c = cellRef.charAt(i)) >= 'A' && c <= 'Z') {
      columnNumber = (short) (columnNumber * 26 + (c - 'A' + 1));
      i++;
    }

    if (!seenValue) {
      onCell(rowNumber, columnNumber, cellRef, new CellValue(XSSDataType.BLANK, "", null));
      return;
    }

    var stringValue = getStringValue();
    if (dataType == XSSDataType.NUMBER) {
      boolean isInteger = !stringValue.contains(".");
      boolean isDate = DateUtil.isADateFormat(-1, numberFormat);
      if (isInteger && isDate) {
        dataType = XSSDataType.OLE_DATE;
      } else if (isInteger) {
        dataType = XSSDataType.INTEGER;
      } else if (isDate) {
        dataType = XSSDataType.OLE_DATETIME;
      }
    }

    var cellValue = new CellValue(dataType, stringValue, numberFormat);
    onCell(rowNumber, columnNumber, cellRef, cellValue);
  }

  protected void onDimensions(String dimension) {}

  protected void onStartRow(int rowNumber) {}

  protected void onCell(int rowNumber, short columnNumber, String ref, CellValue cellValue) {}

  protected void onSheetEnd() {}
}
