package org.enso.table.excel;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.exceptions.OLE2NotOfficeXmlFileException;
import org.apache.poi.openxml4j.exceptions.OpenXML4JRuntimeException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.openxml4j.opc.PackageAccess;
import org.apache.poi.poifs.filesystem.NotOLE2FileException;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.enso.table.excel.xssfreader.XSSFReaderWorkbook;

/** XLSX (XSSF/SXSSF) format strategy. */
public class XlsxFormatStrategy extends ExcelFormatStrategy {

  @Override
  public Workbook openExisting(File file, boolean writeAccess) throws IOException {
    try {
      PackageAccess access = writeAccess ? PackageAccess.READ_WRITE : PackageAccess.READ;
      OPCPackage pkg = OPCPackage.open(file, access);
      try {
        this.workbook = new XSSFWorkbook(pkg);
      } catch (IOException e) {
        pkg.close();
        throw e;
      }
    } catch (InvalidFormatException | OLE2NotOfficeXmlFileException e) {
      throw new IOException("Invalid XLSX format when opening file: " + file, e);
    }
    return workbook;
  }

  @Override
  public ExcelWorkbookReader getExcelWorkbookReader(File file)
      throws IOException, InterruptedException {
    try {
      return new XSSFReaderWorkbook(file.getAbsolutePath());
    } catch (OLE2NotOfficeXmlFileException | NotOLE2FileException e) {
      throw new IOException(
          "Invalid format encountered when opening the file " + file + " as XLSX.", e);
    }
  }

  @Override
  public Workbook createNew() {
    this.workbook = new SXSSFWorkbook();
    return workbook;
  }

  @Override
  public void saveInPlace() throws IOException {
    if (!(workbook instanceof XSSFWorkbook xssf)) {
      throw new IllegalStateException(
          "Expected XSSFWorkbook for XLSX saveInPlace, got: "
              + (workbook == null ? "null" : workbook.getClass()));
    }
    try {
      xssf.write(null);
    } catch (OpenXML4JRuntimeException e) {
      // Ignore: Workaround for https://bz.apache.org/bugzilla/show_bug.cgi?id=59252
    }
  }

  @Override
  public void saveToStream(OutputStream out) throws IOException {
    if (workbook == null) {
      throw new IllegalStateException("Workbook not initialized");
    }
    workbook.write(out);
  }

  @Override
  public void cleanup() throws IOException {
    if (workbook instanceof SXSSFWorkbook sxssf) {
      sxssf.dispose();
    }
  }
}
