/* ====================================================================
   Licensed to the Apache Software Foundation (ASF) under one or more
   contributor license agreements.  See the NOTICE file distributed with
   this work for additional information regarding copyright ownership.
   The ASF licenses this file to You under the Apache License, Version 2.0
   (the "License"); you may not use this file except in compliance with
   the License.  You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
==================================================================== */

package org.apache.poi.xssf.binary;

import java.io.InputStream;
import java.util.Queue;
import org.apache.poi.ss.usermodel.BuiltinFormats;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.usermodel.ExcelNumberFormat;
import org.apache.poi.ss.usermodel.FormulaError;
import org.apache.poi.ss.usermodel.RichTextString;
import org.apache.poi.ss.util.CellAddress;
import org.apache.poi.util.Internal;
import org.apache.poi.util.LittleEndian;
import org.apache.poi.util.StringUtil;
import org.apache.poi.xssf.eventusermodel.XSSFSheetXMLHandler;
import org.apache.poi.xssf.model.SharedStrings;
import org.apache.poi.xssf.usermodel.XSSFComment;

/**
 * @since 3.16-beta3
 */
@Internal
public class XSSFBSheetHandler extends XSSFBParser {

  private static final int CHECK_ALL_ROWS = -1;

  private final SharedStrings stringsTable;
  private final XSSFBSheetContentsHandler handler;
  private final XSSFBStylesTable styles;
  private final XSSFBCommentsTable comments;
  private final boolean formulasNotResults; // TODO: implement this

  private int lastEndedRow = -1;
  private int lastStartedRow = -1;
  private int currentRow;
  private byte[] rkBuffer = new byte[8];
  private XSSFBCellRange hyperlinkCellRange;
  private StringBuilder xlWideStringBuffer = new StringBuilder();

  private final XSSFBCellHeader cellBuffer = new XSSFBCellHeader();

  /**
   * Creates a handler that forwards native POI cell types to the supplied {@link
   * XSSFBSheetContentsHandler}.
   *
   * <p>Select this overload when the consumer expects the raw cell representation rather than
   * formatted strings.
   *
   * @param is XLSB worksheet stream to parse
   * @param styles table providing cell style and number format metadata
   * @param comments optional comments table, may be {@code null}
   * @param strings shared strings table used by the sheet
   * @param sheetContentsHandler callback receiving native cell events
   * @param formulasNotResults {@code true} to request formulas rather than cached results
   *     (currently not implemented)
   */
  public XSSFBSheetHandler(
      InputStream is,
      XSSFBStylesTable styles,
      XSSFBCommentsTable comments,
      SharedStrings strings,
      XSSFBSheetContentsHandler sheetContentsHandler,
      boolean formulasNotResults) {
    super(is);
    this.styles = styles;
    this.comments = comments;
    this.stringsTable = strings;
    this.handler = sheetContentsHandler;
    this.formulasNotResults = formulasNotResults;
  }

  /**
   * Creates a handler that converts numeric and date cells to formatted strings via {@link
   * DataFormatter}.
   *
   * <p>This variant mirrors the SAX-based API from {@link XSSFSheetXMLHandler} so existing POI
   * consumers can reuse their {@link XSSFSheetXMLHandler.SheetContentsHandler} implementations.
   *
   * @param is XLSB worksheet stream to parse
   * @param styles table providing cell style and number format metadata
   * @param comments optional comments table, may be {@code null}
   * @param strings shared strings table used by the sheet
   * @param sheetContentsHandler callback receiving formatted string values
   * @param dataFormatter formatter applied to numeric and date cells
   * @param formulasNotResults {@code true} to request formulas rather than cached results
   *     (currently not implemented)
   * @see #XSSFBSheetHandler(InputStream, XSSFBStylesTable, XSSFBCommentsTable, SharedStrings,
   *     XSSFBSheetContentsHandler, boolean)
   */
  public XSSFBSheetHandler(
      InputStream is,
      XSSFBStylesTable styles,
      XSSFBCommentsTable comments,
      SharedStrings strings,
      XSSFSheetXMLHandler.SheetContentsHandler sheetContentsHandler,
      DataFormatter dataFormatter,
      boolean formulasNotResults) {
    super(is);
    this.styles = styles;
    this.comments = comments;
    this.stringsTable = strings;
    this.handler = new XSSFBSheetContentsHandlerWrapper(sheetContentsHandler, dataFormatter);
    this.formulasNotResults = formulasNotResults;
  }

  /**
   * Dispatches a parsed XLSB record to the appropriate specialised handler.
   *
   * @param id numeric record identifier supplied by {@link XSSFBParser}
   * @param data raw record payload
   * @throws XSSFBParseException if the record cannot be processed according to the XLSB spec
   * @see XSSFBRecordType
   */
  @Override
  public void handleRecord(int id, byte[] data) throws XSSFBParseException {
    XSSFBRecordType type = XSSFBRecordType.lookup(id);

    switch (type) {
      case BrtRowHdr:
        int rw = XSSFBUtils.castToInt(LittleEndian.getUInt(data, 0));
        if (rw > 0x00100000) { // could make sure this is larger than currentRow, according to spec?
          throw new XSSFBParseException("Row number beyond allowable range: " + rw);
        }
        currentRow = rw;
        checkMissedComments(currentRow);
        startRow(currentRow);
        break;
      case BrtCellIsst:
        handleBrtCellIsst(data);
        break;
      case BrtCellSt: // TODO: needs test
        handleCellSt(data);
        break;
      case BrtCellRk:
        handleCellRk(data);
        break;
      case BrtCellReal:
        handleCellReal(data);
        break;
      case BrtCellBool:
        handleBoolean(data);
        break;
      case BrtCellError:
        handleCellError(data);
        break;
      case BrtCellBlank:
        beforeCellValue(data); // read cell info and check for missing comments
        break;
      case BrtFmlaString:
        handleFmlaString(data);
        break;
      case BrtFmlaNum:
        handleFmlaNum(data);
        break;
      case BrtFmlaError:
        handleCellError(data);
        break;
      // TODO: All the PCDI and PCDIA
      case BrtEndSheetData:
        checkMissedComments(CHECK_ALL_ROWS);
        endRow(lastStartedRow);
        break;
      case BrtBeginHeaderFooter:
        handleHeaderFooter(data);
        break;
    }
  }

  private void beforeCellValue(byte[] data) {
    XSSFBCellHeader.parse(data, 0, currentRow, cellBuffer);
    checkMissedComments(currentRow, cellBuffer.getColNum());
  }

  private void handleStringCellValue(String formattedValue) {
    CellAddress cellAddress = getCellAddress();
    XSSFBComment comment = getCellComment(cellAddress);
    handler.stringCell(cellAddress.formatAsString(), formattedValue, comment);
  }

  private void handleDoubleCellValue(double val) {
    CellAddress cellAddress = getCellAddress();
    XSSFBComment comment = getCellComment(cellAddress);
    ExcelNumberFormat nf = getExcelNumberFormat();
    handler.doubleCell(cellAddress.formatAsString(), val, comment, nf);
  }

  private CellAddress getCellAddress() {
    return new CellAddress(currentRow, cellBuffer.getColNum());
  }

  private XSSFBComment getCellComment(CellAddress cellAddress) {
    XSSFBComment comment = null;
    if (comments != null) {
      comment = comments.get(cellAddress);
    }
    return comment;
  }

  private ExcelNumberFormat getExcelNumberFormat() {
    var styleIdx = cellBuffer.getStyleIdx();
    String formatString = styles.getNumberFormatString(styleIdx);
    short styleIndex = styles.getNumberFormatIndex(styleIdx);
    // for now, if formatString is null, silently punt
    // and use "General".  Not the best behavior,
    // but we're doing it now in the streaming and non-streaming
    // extractors for xlsx.  See BUG-61053
    if (formatString == null) {
      formatString = BuiltinFormats.getBuiltinFormat(0);
      styleIndex = 0;
    }
    return new ExcelNumberFormat(styleIndex, formatString);
  }

  private void handleFmlaNum(byte[] data) {
    beforeCellValue(data);
    // xNum
    double val = LittleEndian.getDouble(data, XSSFBCellHeader.length);
    handleDoubleCellValue(val);
  }

  private void handleCellSt(byte[] data) {
    beforeCellValue(data);
    xlWideStringBuffer.setLength(0);
    XSSFBUtils.readXLWideString(data, XSSFBCellHeader.length, xlWideStringBuffer);
    handleStringCellValue(xlWideStringBuffer.toString());
  }

  private void handleFmlaString(byte[] data) {
    beforeCellValue(data);
    xlWideStringBuffer.setLength(0);
    XSSFBUtils.readXLWideString(data, XSSFBCellHeader.length, xlWideStringBuffer);
    handleStringCellValue(xlWideStringBuffer.toString());
  }

  private void handleCellError(byte[] data) {
    beforeCellValue(data);
    int bErr = data[XSSFBCellHeader.length] & 0xFF;
    FormulaError fe;
    try {
      fe = FormulaError.forInt(bErr);
    } catch (IllegalArgumentException e) {
      fe = null;
    }
    CellAddress cellAddress = getCellAddress();
    XSSFBComment comment = getCellComment(cellAddress);
    handler.errorCell(cellAddress.formatAsString(), fe, comment);
  }

  private void handleBoolean(byte[] data) {
    beforeCellValue(data);
    boolean val = data[XSSFBCellHeader.length] == 1;
    CellAddress cellAddress = getCellAddress();
    XSSFBComment comment = getCellComment(cellAddress);
    handler.booleanCell(cellAddress.formatAsString(), val, comment);
  }

  private void handleCellReal(byte[] data) {
    beforeCellValue(data);
    // xNum
    double val = LittleEndian.getDouble(data, XSSFBCellHeader.length);
    handleDoubleCellValue(val);
  }

  private void handleCellRk(byte[] data) {
    beforeCellValue(data);
    double val = rkNumber(data, XSSFBCellHeader.length);
    handleDoubleCellValue(val);
  }

  private void handleBrtCellIsst(byte[] data) {
    beforeCellValue(data);
    int idx = XSSFBUtils.castToInt(LittleEndian.getUInt(data, XSSFBCellHeader.length));
    RichTextString rtss = stringsTable.getItemAt(idx);
    handleStringCellValue(rtss.getString());
  }

  private void handleHeaderFooter(byte[] data) {
    XSSFBHeaderFooters headerFooter = XSSFBHeaderFooters.parse(data);
    outputHeaderFooter(headerFooter.getHeader());
    outputHeaderFooter(headerFooter.getFooter());
    outputHeaderFooter(headerFooter.getHeaderEven());
    outputHeaderFooter(headerFooter.getFooterEven());
    outputHeaderFooter(headerFooter.getHeaderFirst());
    outputHeaderFooter(headerFooter.getFooterFirst());
  }

  private void outputHeaderFooter(XSSFBHeaderFooter headerFooter) {
    String text = headerFooter.getString();
    if (StringUtil.isNotBlank(text)) {
      handler.headerFooter(text, headerFooter.isHeader(), headerFooter.getHeaderFooterTypeLabel());
    }
  }

  // at start of next cell or end of row, return the cellAddress if it equals currentRow and col
  private void checkMissedComments(int currentRow, int colNum) {
    if (comments == null) {
      return;
    }
    Queue<CellAddress> queue = comments.getAddresses();
    while (!queue.isEmpty()) {
      CellAddress cellAddress = queue.peek();
      if (cellAddress.getRow() == currentRow && cellAddress.getColumn() < colNum) {
        cellAddress = queue.remove();
        dumpEmptyCellComment(cellAddress, comments.get(cellAddress));
      } else if (cellAddress.getRow() == currentRow && cellAddress.getColumn() == colNum) {
        queue.remove();
        return;
      } else if (cellAddress.getRow() == currentRow && cellAddress.getColumn() > colNum) {
        return;
      } else if (cellAddress.getRow() > currentRow) {
        return;
      }
    }
  }

  // check for anything from rows before
  private void checkMissedComments(int currentRow) {
    if (comments == null) {
      return;
    }
    Queue<CellAddress> queue = comments.getAddresses();
    int lastInterpolatedRow = -1;
    while (!queue.isEmpty()) {
      CellAddress cellAddress = queue.peek();
      if (currentRow == CHECK_ALL_ROWS || cellAddress.getRow() < currentRow) {
        cellAddress = queue.remove();
        if (cellAddress.getRow() != lastInterpolatedRow) {
          startRow(cellAddress.getRow());
        }
        dumpEmptyCellComment(cellAddress, comments.get(cellAddress));
        lastInterpolatedRow = cellAddress.getRow();
      } else {
        break;
      }
    }
  }

  private void startRow(int row) {
    if (row == lastStartedRow) {
      return;
    }

    if (lastStartedRow != lastEndedRow) {
      endRow(lastStartedRow);
    }
    handler.startRow(row);
    lastStartedRow = row;
  }

  private void endRow(int row) {
    if (lastEndedRow == row) {
      return;
    }
    handler.endRow(row);
    lastEndedRow = row;
  }

  private void dumpEmptyCellComment(CellAddress cellAddress, XSSFBComment comment) {
    handler.stringCell(cellAddress.formatAsString(), null, comment);
  }

  private double rkNumber(byte[] data, int offset) {
    // see 2.5.122
    byte b0 = data[offset];
    boolean numDivBy100 = ((b0 & 1) == 1); // else as is
    boolean floatingPoint = ((b0 >> 1 & 1) == 0); // else signed integer

    // unset highest 2 bits
    b0 &= ~1;
    b0 &= ~(1 << 1);

    rkBuffer[4] = b0;
    System.arraycopy(data, offset + 1, rkBuffer, 5, 3);
    double d = 0.0;
    if (floatingPoint) {
      d = LittleEndian.getDouble(rkBuffer);
    } else {
      int rawInt = LittleEndian.getInt(rkBuffer, 4);
      d = rawInt >> 2; // divide by 4/shift bits coz 30 bit int, not 32
    }
    d = (numDivBy100) ? d / 100 : d;
    return d;
  }

  /**
   * Receives streaming callbacks while {@link XSSFBSheetHandler} parses an XLSB sheet.
   *
   * <p>Implementations follow the same contract as Apache POI's SAX sheet handler but operate on
   * the binary file format exposed by {@link XSSFBSheetHandler}.
   *
   * @see XSSFBSheetHandler
   * @see XSSFSheetXMLHandler.SheetContentsHandler
   */
  public interface XSSFBSheetContentsHandler {
    /**
     * Signals that a row has started before any of its cells are delivered.
     *
     * @param rowNum zero-based row index
     * @see #endRow(int)
     */
    void startRow(int rowNum);

    /**
     * Signals that a row has ended after all of its cells and comments were processed.
     *
     * @param rowNum zero-based row index
     * @see #startRow(int)
     */
    void endRow(int rowNum);

    /**
     * Handles a cell that resolves to a string value, possibly representing a comment-only cell.
     *
     * @param cellReference A1-style cell address
     * @param value string contents, or {@code null} if only a comment is present
     * @param comment associated comment, or {@code null} if absent
     *     <p>Sheets that have missing or empty cells may result in sparse calls to <code>cell
     *     </code>. See the code in <code>
     * poi-examples/src/main/java/org/apache/poi/xssf/eventusermodel/XLSX2CSV.java</code> for an
     *     example of how to handle this scenario.
     * @see #doubleCell(String, double, XSSFComment, ExcelNumberFormat)
     */
    void stringCell(String cellReference, String value, XSSFComment comment);

    /**
     * Handles a numeric cell while providing the corresponding {@link ExcelNumberFormat}.
     *
     * @param cellReference A1-style cell address
     * @param value numeric value extracted from the sheet
     * @param comment associated comment, or {@code null} if absent
     * @param nf number format describing how the value should be rendered
     *     <p>Sheets that have missing or empty cells may result in sparse calls to <code>cell
     *     </code>. See the code in <code>
     * poi-examples/src/main/java/org/apache/poi/xssf/eventusermodel/XLSX2CSV.java</code> for an
     *     example of how to handle this scenario.
     * @see #stringCell(String, String, XSSFComment)
     */
    void doubleCell(String cellReference, double value, XSSFComment comment, ExcelNumberFormat nf);

    /**
     * Handles a boolean cell.
     *
     * @param cellReference A1-style cell address
     * @param value boolean value stored in the cell
     * @param comment associated comment, or {@code null} if absent
     *     <p>Sheets that have missing or empty cells may result in sparse calls to <code>cell
     *     </code>. See the code in <code>
     * poi-examples/src/main/java/org/apache/poi/xssf/eventusermodel/XLSX2CSV.java</code> for an
     *     example of how to handle this scenario.
     * @see #stringCell(String, String, XSSFComment)
     */
    void booleanCell(String cellReference, boolean value, XSSFComment comment);

    /**
     * Handles a cell that evaluates to an error.
     *
     * @param cellReference A1-style cell address
     * @param fe mapped {@link FormulaError}, or {@code null} when the error code is unknown
     * @param comment associated comment, or {@code null} if absent
     *     <p>Sheets that have missing or empty cells may result in sparse calls to <code>cell
     *     </code>. See the code in <code>
     * poi-examples/src/main/java/org/apache/poi/xssf/eventusermodel/XLSX2CSV.java</code> for an
     *     example of how to handle this scenario.
     * @see FormulaError
     */
    void errorCell(String cellReference, FormulaError fe, XSSFComment comment);

    /**
     * Receives header or footer text encountered in the sheet.
     *
     * @param text resolved header or footer text
     * @param isHeader {@code true} when the text belongs to a header, otherwise {@code false}
     * @param tagName POI-internal tag representing the header or footer section
     * @see #endSheet()
     */
    void headerFooter(String text, boolean isHeader, String tagName);

    /**
     * Signals that the sheet has been completely processed.
     *
     * @see #startRow(int)
     */
    void endSheet();
  }

  /**
   * Bridges a {@link XSSFSheetXMLHandler.SheetContentsHandler} to the {@link
   * XSSFBSheetContentsHandler} contract.
   *
   * @see XSSFSheetXMLHandler
   */
  private final class XSSFBSheetContentsHandlerWrapper implements XSSFBSheetContentsHandler {
    private final XSSFSheetXMLHandler.SheetContentsHandler delegate;
    private final DataFormatter dataFormatter;

    /**
     * Creates a wrapper that forwards events to the XML sheet handler while formatting numeric
     * cells.
     *
     * @param delegate target handler compatible with the XML streaming API
     * @param dataFormatter formatter used for numeric and date cell rendering
     */
    XSSFBSheetContentsHandlerWrapper(
        XSSFSheetXMLHandler.SheetContentsHandler delegate, DataFormatter dataFormatter) {
      this.delegate = delegate;
      this.dataFormatter = dataFormatter;
    }

    @Override
    public void startRow(int rowNum) {
      delegate.startRow(rowNum);
    }

    @Override
    public void endRow(int rowNum) {
      delegate.endRow(rowNum);
    }

    @Override
    public void stringCell(String cellReference, String value, XSSFComment comment) {
      delegate.cell(cellReference, value, comment);
    }

    @Override
    public void doubleCell(
        String cellReference, double value, XSSFComment comment, ExcelNumberFormat nf) {
      String formattedValue =
          dataFormatter.formatRawCellContents(value, nf.getIdx(), nf.getFormat());
      delegate.cell(cellReference, formattedValue, comment);
    }

    @Override
    public void booleanCell(String cellReference, boolean value, XSSFComment comment) {
      delegate.cell(cellReference, Boolean.toString(value), comment);
    }

    @Override
    public void errorCell(String cellReference, FormulaError fe, XSSFComment comment) {
      String errorText = fe != null ? fe.getString() : "ERROR";
      delegate.cell(cellReference, errorText, comment);
    }

    @Override
    public void headerFooter(String text, boolean isHeader, String tagName) {
      delegate.headerFooter(text, isHeader, tagName);
    }

    @Override
    public void endSheet() {
      delegate.endSheet();
    }
  }

  /** You need to implement this to handle the results of the sheet parsing. */
  public interface SheetContentsHandler extends XSSFSheetXMLHandler.SheetContentsHandler {
    /**
     * A cell, with the given formatted value (may be null), a url (may be null), a toolTip (may be
     * null) and possibly a comment (may be null), was encountered
     */
    void hyperlinkCell(
        String cellReference,
        String formattedValue,
        String url,
        String toolTip,
        XSSFComment comment);
  }
}
