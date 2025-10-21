package org.enso.table.excel;

import static org.enso.table.excel.ExcelUtils.formatNumericValue;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.exceptions.OpenXML4JException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.openxml4j.opc.PackageAccess;
import org.apache.poi.ss.usermodel.ExcelNumberFormat;
import org.apache.poi.ss.usermodel.FormulaError;
import org.apache.poi.xssf.binary.XSSFBSharedStringsTable;
import org.apache.poi.xssf.binary.XSSFBSheetHandler;
import org.apache.poi.xssf.binary.XSSFBSheetHandler.XSSFBSheetContentsHandler;
import org.apache.poi.xssf.binary.XSSFBStylesTable;
import org.apache.poi.xssf.eventusermodel.XSSFBReader;
import org.apache.poi.xssf.model.SharedStrings;
import org.apache.poi.xssf.usermodel.XSSFComment;

/** An Excel workbook reader for XLSB files. */
public class XlsbExcelWorkbookReader implements ExcelWorkbookReader {

  private final File file;
  XSSFBReader xssfbReader;
  private final List<SheetHolder> sheets = new ArrayList<>();

  public XlsbExcelWorkbookReader(File file) throws IOException, InvalidFormatException {
    this.file = file;
    try (OPCPackage opcPackage = OPCPackage.open(file, PackageAccess.READ)) {
      xssfbReader = new XSSFBReader(opcPackage);

      SharedStrings sharedStrings = null;
      try {
        sharedStrings = new XSSFBSharedStringsTable(opcPackage);
      } catch (Exception e) {
        throw new IOException(
            "No shared strings table found or error reading it: " + e.getMessage());
      }

      XSSFBStylesTable stylesTable = null;
      try {
        stylesTable = xssfbReader.getXSSFBStylesTable();
      } catch (Exception e) {
        throw new IOException("No styles table found or error reading it: " + e.getMessage());
      }

      XSSFBReader.SheetIterator sheetsData =
          (XSSFBReader.SheetIterator) xssfbReader.getSheetsData();
      while (sheetsData.hasNext()) {
        try (InputStream sheetInputStream = sheetsData.next()) {
          String sheetName = sheetsData.getSheetName();

          // Create a custom sheet contents handler to capture cell data
          XlsbSheetContentsHandler contentsHandler = new XlsbSheetContentsHandler();

          // Create the sheet handler
          XSSFBSheetHandler sheetHandler =
              new XSSFBSheetHandler(
                  sheetInputStream,
                  stylesTable,
                  null, // comments table - not implemented for simplicity
                  sharedStrings,
                  contentsHandler,
                  false // formulasNotResults
                  );

          // Parse the sheet
          sheetHandler.parse();

          sheets.add(new SheetHolder(sheetName, contentsHandler));
        } catch (Exception e) {
          throw new IOException("Error parsing sheet: " + e.getMessage());
        }
      }
    } catch (InvalidFormatException e) {
      throw new IOException("Invalid XLSB format when opening file: " + file, e);
    } catch (OpenXML4JException e) {
      throw new IOException("Error processing XLSB file: " + file, e);
    }
  }

  @Override
  public int getNumberOfSheets() {
    return sheets.size();
  }

  @Override
  public int getSheetIndex(String name) {
    for (int i = 0; i < sheets.size(); i++) {
      if (sheets.get(i).name.equals(name)) {
        return i;
      }
    }
    return -1;
  }

  @Override
  public String getSheetName(int sheet) {
    return sheets.get(sheet).name;
  }

  @Override
  public int getNumberOfNames() {
    // Named ranges are currently not supported in the underlying library.
    return 0;
  }

  private static final class SheetHolder {
    final String name;
    final XlsbSheetContentsHandler handler;

    SheetHolder(String name, XlsbSheetContentsHandler handler) {
      this.name = name;
      this.handler = handler;
    }
  }

  @Override
  public String[] getRangeNames() {
    // Named ranges are currently not supported in the underlying library.
    return new String[0];
  }

  @Override
  public String getNameFormula(String name) {
    // Named ranges are currently not supported in the underlying library.
    return null;
  }

  @Override
  public ExcelSheetReader getSheetAt(int sheetIndex) {
    if (sheetIndex < 0 || sheetIndex >= sheets.size()) {
      throw new IndexOutOfBoundsException(
          "Requested sheet index " + sheetIndex + " is out of bounds for workbook " + file);
    }
    var holder = sheets.get(sheetIndex);
    return new XlsbExcelSheetReader(sheetIndex, holder.name, holder.handler);
  }

  @Override
  public void close() throws IOException {
    // Nothing to close
  }

  /**
   * Custom implementation of SheetContentsHandler to capture and display cell data from XLSB files.
   */
  static class XlsbSheetContentsHandler implements XSSFBSheetContentsHandler {
    private final List<RowData> rows = new ArrayList<>();
    private List<Object> currentRow = new ArrayList<>();
    private int currentRowIndex = -1;
    private int maxColumns = 0;
    private int firstRowIndex = -1;
    private int lastRowIndex = -1;
    private int currentRowFirstColumnIndex = Integer.MAX_VALUE;
    private int currentRowLastColumnIndex = -1;

    @Override
    public void startRow(int rowNum) {
      while (currentRowIndex + 1 < rowNum) {
        rows.add(null);
        currentRowIndex++;
      }

      currentRow = new ArrayList<>();
      currentRowIndex = rowNum;
      currentRowFirstColumnIndex = Integer.MAX_VALUE;
      currentRowLastColumnIndex = -1;

      if (firstRowIndex == -1 || rowNum < firstRowIndex) {
        firstRowIndex = rowNum;
      }
      if (rowNum > lastRowIndex) {
        lastRowIndex = rowNum;
      }
    }

    @Override
    public void endRow(int rowNum) {
      var rowCopy = Collections.unmodifiableList(new ArrayList<>(currentRow));
      int firstColumn = currentRowLastColumnIndex >= 0 ? currentRowFirstColumnIndex + 1 : 0;
      int lastColumn = currentRowLastColumnIndex >= 0 ? currentRowLastColumnIndex + 1 : 0;
      rows.add(new RowData(rowCopy, firstColumn, lastColumn));
      maxColumns = Math.max(maxColumns, rowCopy.size());
    }

    private void setCellValue(String cellReference, Object value) {
      int colIndex = getColumnIndex(cellReference);
      while (currentRow.size() <= colIndex) {
        currentRow.add(null);
      }
      currentRow.set(colIndex, value);
      currentRowFirstColumnIndex = Math.min(currentRowFirstColumnIndex, colIndex);
      currentRowLastColumnIndex = Math.max(currentRowLastColumnIndex, colIndex);
    }

    @Override
    public void stringCell(String cellReference, String value, XSSFComment comment) {
      setCellValue(cellReference, value);
    }

    @Override
    public void doubleCell(
        String cellReference, double value, XSSFComment comment, ExcelNumberFormat nf) {
      var val = formatNumericValue(value, nf, false);
      setCellValue(cellReference, val);
    }

    @Override
    public void booleanCell(String cellReference, boolean value, XSSFComment comment) {
      setCellValue(cellReference, value);
    }

    @Override
    public void errorCell(String cellReference, FormulaError fe, XSSFComment comment) {
      setCellValue(cellReference, null);
    }

    @Override
    public void headerFooter(String text, boolean isHeader, String tagName) {
      // We don't currently read headers/footers
    }

    @Override
    public void endSheet() {
      // No additional actions needed at the end of the sheet
    }

    private int getColumnIndex(String cellReference) {
      if (cellReference == null || cellReference.isEmpty()) {
        return 0;
      }

      int colIndex = 0;
      for (int i = 0; i < cellReference.length(); i++) {
        char c = cellReference.charAt(i);
        if (Character.isDigit(c)) {
          break;
        }
        colIndex = colIndex * 26 + (c - 'A' + 1);
      }
      return colIndex - 1;
    }

    int getFirstRowNumber() {
      return firstRowIndex >= 0 ? firstRowIndex + 1 : 0;
    }

    int getLastRowNumber() {
      return lastRowIndex >= 0 ? lastRowIndex + 1 : 0;
    }

    RowData getRowData(int rowNumber) {
      int index = rowNumber - 1;
      if (index < 0 || index >= rows.size()) {
        return null;
      }
      return rows.get(index);
    }

    static final class RowData {
      private final List<Object> values;
      private final int firstColumn;
      private final int lastColumn;

      RowData(List<Object> values, int firstColumn, int lastColumn) {
        this.values = values;
        this.firstColumn = firstColumn;
        this.lastColumn = lastColumn;
      }

      List<Object> values() {
        return values;
      }

      int firstColumn() {
        return firstColumn;
      }

      int lastColumn() {
        return lastColumn;
      }
    }
  }
}
