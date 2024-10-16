package org.enso.table.excel.xssfreader;

import static org.apache.poi.xssf.usermodel.XSSFRelation.NS_SPREADSHEETML;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.temporal.Temporal;
import org.apache.poi.ss.usermodel.BuiltinFormats;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.xssf.model.SharedStrings;
import org.apache.poi.xssf.model.Styles;
import org.apache.poi.xssf.usermodel.XSSFRichTextString;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

/** Based on the XSSFSheetXMLHandler class from Apache POI. */
public class XSSFReaderSheetXMLHandler extends DefaultHandler {
  private final Styles styleTables;
  private final SharedStrings sharedStrings;

  public enum XSSDataType {
    BOOL,
    ERROR,
    FORMULA,
    INLINE_STRING,
    SST_STRING,
    NUMBER
  }

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
  private int numberFormatIndex = -1;
  private String numberFormat = null;

  public XSSFReaderSheetXMLHandler(Styles styles, SharedStrings strings) {
    this.styleTables = styles;
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
        case "row": // Row
          String rowNumStr = attributes.getValue("r");
          rowNumber = rowNumStr == null ? nextRowNumber : Integer.parseInt(rowNumStr);
          onStartRow(rowNumber);
          break;
        case "c": // Cell
          cellRef = attributes.getValue("r");

          String cellType = attributes.getValue("t");
          if (cellType == null) {
            cellType = "n"; // Number is default
          }
          dataType =
              switch (cellType) {
                case "b" -> XSSDataType.BOOL;
                case "e" -> XSSDataType.ERROR;
                case "inlineStr" -> XSSDataType.INLINE_STRING;
                case "s" -> XSSDataType.SST_STRING;
                case "str" -> XSSDataType.FORMULA;
                default -> XSSDataType.NUMBER;
              };

          // Read the format for NUMBER or FORMULA
          numberFormatIndex = -1;
          numberFormat = null;
          if (dataType == XSSDataType.NUMBER) {
            String cellStyleStr = attributes.getValue("s");
            if (cellStyleStr != null) {
              short styleIndex = (short) Integer.parseInt(cellStyleStr);
              var style = styleTables.getStyleAt(styleIndex);
              if (style != null) {
                numberFormatIndex = style.getDataFormat();
                numberFormat = style.getDataFormatString();
                if (numberFormat == null) {
                  numberFormat = BuiltinFormats.getBuiltinFormat(numberFormatIndex);
                }
              }
            }
          }
          break;
        case "is": // Inline String
          isIsOpen = true;
          break;
        case "f": // Formula
          if (dataType == XSSDataType.NUMBER) {
            dataType = XSSDataType.FORMULA;
          }
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
      if (!isIsOpen) {
        outputCellValue();
      }
    } else {
      switch (localName) {
        case "row":
          nextRowNumber = rowNumber + 1;
          rowNumber = -1;
          break;
        case "is":
          isIsOpen = false;
          outputCellValue();
          break;
        case "sheetData":
          // End of sheet data
          onSheetEnd();
          break;
      }
    }
  }

  public record CellValue(XSSDataType dataType, String strValue, int formatIndex, String format) {
    static final LocalDate EPOC_1900 = LocalDate.of(1899, 12, 30);
    static final long MILLIS_PER_DAY = 24 * 60 * 60 * 1000;

    static Temporal fromExcelDate(String text) {
      var idx = text.indexOf('.');

      // Get the date and time part part
      long date;
      double fraction;
      if (idx == -1) {
        date = Long.parseLong(text);
        fraction = 0;
      } else {
        var dbl = Double.parseDouble(text.substring(idx));
        date = (long)Math.floor(dbl);
        fraction = dbl - date;
      }

      // Excel treats 1900-02-29 as a valid date, which it isn't
      if (date == 60) {
        return null;
      }

      var localDate = date == 0 ? EPOC_1900 : EPOC_1900.plusDays((date < 61 ? 1 : 0) + date);
      // Return as a LocalDate unless 0 then Midnight
      if (fraction == 0) {
        return date == 0 ? LocalTime.MIDNIGHT : localDate;
      }

      // Get the time part (fractional part)
      var millis = (long) (fraction * MILLIS_PER_DAY + 0.5);
      var time = LocalTime.ofNanoOfDay(millis * 1_000_000);
      return date == 0 ? time : localDate.atTime(time);
    }

    public boolean getBooleanValue() {
      return strValue.charAt(0) == '1';
    }

    public boolean isInteger() {
      for (int i = 0; i < strValue.length(); i++) {
        var c = strValue.charAt(i);
        if (c == '.' || c == 'E' || c == 'e') {
          return false;
        }
      }
      return false;
    }

    public long getLongValue() {
      return Long.parseLong(strValue);
    }

    public double getNumberValue() {
      return Double.parseDouble(strValue);
    }

    public Object getValue(boolean resolveDates) {
      return switch (dataType()) {
        case BOOL -> getBooleanValue();
        case ERROR -> null;
        case INLINE_STRING, SST_STRING -> strValue();
        default -> {
          if (resolveDates && DateUtil.isADateFormat(formatIndex, format)) {
            var datetime = fromExcelDate(strValue());
            yield datetime instanceof LocalDateTime
                ? ((LocalDateTime) datetime).atZone(ZoneId.systemDefault())
                : datetime;
          }
          yield isInteger() ? getLongValue() : getNumberValue();
        }
      };
    }
  }

  public String getStringValue() {
    if (dataType == XSSDataType.SST_STRING) {
      int idx = Integer.parseInt(value.toString());
      var ss = sharedStrings.getItemAt(idx);
      return ss.toString();
    } else if (dataType == XSSDataType.INLINE_STRING) {
      return new XSSFRichTextString(value.toString()).toString();
    }
    return value.toString();
  }

  private void outputCellValue() {
    var cellValue = new CellValue(dataType, getStringValue(), numberFormatIndex, numberFormat);
    onCell(rowNumber, cellRef, cellValue);
  }

  protected void onDimensions(String dimension) {}

  protected void onStartRow(int rowNumber) {}

  protected void onCell(int rowNumber, String ref, CellValue cellValue) {}

  protected void onSheetEnd() {}
}
