package org.enso.table.excel;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.openxml4j.exceptions.OLE2NotOfficeXmlFileException;
import org.apache.poi.poifs.filesystem.OfficeXmlFileException;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;
import org.apache.poi.ss.usermodel.Name;
import org.apache.poi.ss.usermodel.Workbook;

/** XLS (HSSF) format strategy. */
public class XlsFormatStrategy extends ExcelFormatStrategy {

  @Override
  public Workbook openExisting(File file, boolean writeAccess) throws IOException {
    try {
      boolean readOnly = !writeAccess;
      POIFSFileSystem fs = new POIFSFileSystem(file, readOnly);
      try {
        this.workbook = new HSSFWorkbook(fs);
      } catch (IOException e) {
        fs.close();
        throw e;
      }
    } catch (OfficeXmlFileException | OLE2NotOfficeXmlFileException e) {
      throw new IOException("Invalid XLS format when opening file: " + file, e);
    }
    return workbook;
  }

  @Override
  public ExcelWorkbookReader getExcelWorkbookReader(File file)
      throws IOException, InterruptedException {
    openExisting(file, false);
    return new ExcelWorkbookReaderFromPOIUserModel(workbook);
  }

  @Override
  public Workbook createNew() {
    this.workbook = new HSSFWorkbook();
    return workbook;
  }

  @Override
  public void saveInPlace() throws IOException {
    if (!(workbook instanceof HSSFWorkbook hssf)) {
      throw new IllegalStateException(
          "Expected HSSFWorkbook for XLS saveInPlace, got: "
              + (workbook == null ? "null" : workbook.getClass()));
    }
    hssf.write();
  }

  @Override
  public void saveToStream(OutputStream out) throws IOException {
    if (workbook == null) {
      throw new IllegalStateException("Workbook not initialized");
    }
    workbook.write(out);
  }

  // ** Wrap a Workbook object in the interface. */
  record ExcelWorkbookReaderFromPOIUserModel(org.apache.poi.ss.usermodel.Workbook workbook)
      implements ExcelWorkbookReader {
    @Override
    public int getNumberOfSheets() {
      return workbook.getNumberOfSheets();
    }

    @Override
    public int getSheetIndex(String name) {
      return workbook.getSheetIndex(name);
    }

    @Override
    public String getSheetName(int sheet) {
      return workbook.getSheetName(sheet);
    }

    @Override
    public int getNumberOfNames() {
      return workbook.getNumberOfNames();
    }

    @Override
    public String[] getRangeNames() {
      var names = workbook.getAllNames();
      return names.stream().map(Name::getNameName).toArray(String[]::new);
    }

    @Override
    public String getNameFormula(String name) {
      var namedRange = workbook.getName(name);
      return namedRange == null ? null : namedRange.getRefersToFormula();
    }

    @Override
    public ExcelSheetReader getSheetAt(int sheetIndex) {
      return ExcelSheetReader.forPOIUserModel(workbook, sheetIndex);
    }

    @Override
    public void close() throws IOException {
      workbook.close();
    }
  }
}
