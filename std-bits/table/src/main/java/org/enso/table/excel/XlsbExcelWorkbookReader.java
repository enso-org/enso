package org.enso.table.excel;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.exceptions.OpenXML4JException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.openxml4j.opc.PackageAccess;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.xssf.binary.XSSFBSharedStringsTable;
import org.apache.poi.xssf.binary.XSSFBSheetHandler;
import org.apache.poi.xssf.binary.XSSFBStylesTable;
import org.apache.poi.xssf.eventusermodel.XSSFBReader;
import org.apache.poi.xssf.eventusermodel.XSSFSheetXMLHandler;
import org.apache.poi.xssf.model.SharedStrings;
import org.apache.poi.xssf.usermodel.XSSFComment;

/**
 * Skeleton reader for XLSB workbooks.
 *
 * <p>The implementation will be filled in when XLSB support is added.
 */
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
            System.out.println(e.getMessage() + "No shared strings table found or error reading it: ");
        }

        XSSFBStylesTable stylesTable = null;
        try {
            stylesTable = xssfbReader.getXSSFBStylesTable();
        } catch (Exception e) {
            System.out.println("No styles table found or error reading it: " + e.getMessage());
        }

        XSSFBReader.SheetIterator sheetsData = (XSSFBReader.SheetIterator) xssfbReader.getSheetsData();  
        while (sheetsData.hasNext()) {
                try (InputStream sheetInputStream = sheetsData.next()) {
                    String sheetName = sheetsData.getSheetName();
                    
                    // Create a custom sheet contents handler to capture cell data
                    XlsbSheetContentsHandler contentsHandler = new XlsbSheetContentsHandler();
                    
                    // Create data formatter for cell values
                    DataFormatter dataFormatter = new DataFormatter();
                    
                    // Create the sheet handler
                    XSSFBSheetHandler sheetHandler = new XSSFBSheetHandler(
                        sheetInputStream,
                        stylesTable,
                        null, // comments table - not implemented for simplicity
                        sharedStrings,
                        contentsHandler,
                        dataFormatter,
                        false // formulasNotResults
                    );
                    
                    // Parse the sheet
                    sheetHandler.parse();
                    
                    sheets.add(new SheetHolder(sheetName, contentsHandler));                   
                } catch (Exception e) {
                    System.err.println("Error parsing sheet: " + e.getMessage());
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
    throw notImplemented();
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
    throw notImplemented();
  }

  @Override
  public String getNameFormula(String name) {
    throw notImplemented();
  }

  @Override
  public ExcelSheetReader getSheetAt(int sheetIndex) {
    if (sheetIndex < 0 || sheetIndex >= sheets.size()) {
      throw new IndexOutOfBoundsException(
          "Requested sheet index " + sheetIndex + " is out of bounds for workbook " + file);
    }
    var holder = sheets.get(sheetIndex);
    return new XlsbExcelSheetReader(sheetIndex, holder.name);
  }

  @Override
  public void close() throws IOException {
    throw notImplemented();
  }

  private UnsupportedOperationException notImplemented() {
    return new UnsupportedOperationException("XLSB support is not implemented yet for " + file);
  }

  /**
     * Custom implementation of SheetContentsHandler to capture and display cell data from XLSB files.
     */
    private static class XlsbSheetContentsHandler implements XSSFSheetXMLHandler.SheetContentsHandler {
        private final List<List<String>> rows = new ArrayList<>();
        private List<String> currentRow = new ArrayList<>();
        private int currentRowIndex = -1;
        private int maxColumns = 0;
        
        @Override
        public void startRow(int rowNum) {
            // If we skipped rows, add empty rows
            while (currentRowIndex + 1 < rowNum) {
                rows.add(new ArrayList<>());
                currentRowIndex++;
            }
            
            currentRow = new ArrayList<>();
            currentRowIndex = rowNum;
        }
        
        @Override
        public void endRow(int rowNum) {
            rows.add(new ArrayList<>(currentRow));
            maxColumns = Math.max(maxColumns, currentRow.size());
        }
        
        @Override
        public void cell(String cellReference, String formattedValue, XSSFComment comment) {
            if (formattedValue == null) {
                formattedValue = "";
            }
            
            // Parse cell reference to get column index
            int colIndex = getColumnIndex(cellReference);
            
            // Expand currentRow if necessary
            while (currentRow.size() <= colIndex) {
                currentRow.add("");
            }
            
            currentRow.set(colIndex, formattedValue);
        }
        
        @Override
        public void headerFooter(String text, boolean isHeader, String tagName) {
            // Not implemented for this example
        }
        
        /**
         * Extract column index from cell reference (e.g., "A1" -> 0, "B1" -> 1)
         */
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
            return colIndex - 1; // Convert to 0-based index
        }
        
        /**
         * Print the captured sheet data
         */
        public void printResults() {
            System.out.println("Number of rows: " + rows.size());
            
            for (int i = 0; i < rows.size(); i++) {
                List<String> row = rows.get(i);
                StringBuilder rowData = new StringBuilder();
                rowData.append("Row ").append(i).append(": ");
                
                // Print all columns up to maxColumns to maintain alignment
                for (int j = 0; j < Math.max(row.size(), maxColumns); j++) {
                    String cellValue = (j < row.size()) ? row.get(j) : "";
                    if (cellValue == null) cellValue = "";
                    rowData.append("[").append(cellValue).append("] ");
                }
                
                System.out.println(rowData.toString());
            }
        }
    }
}
