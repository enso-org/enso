package org.enso.table.excel;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import org.apache.poi.ss.usermodel.Workbook;

/** XLSB format strategy. Currently limited to read-only operations. */
public class XlsbFormatStrategy extends ExcelFormatStrategy {

  private static UnsupportedOperationException writeUnsupported() {
    return new UnsupportedOperationException("Writing XLSB files is not supported.");
  }

  @Override
  public Workbook openExisting(File file, boolean writeAccess) throws IOException {
    if (writeAccess) {
      throw writeUnsupported();
    }
    throw new UnsupportedOperationException(
        "Reading XLSB files via the user model is not yet implemented. Use getExcelWorkbookReader.");
  }

  @Override
  public Workbook createNew() {
    throw writeUnsupported();
  }

  @Override
  public void saveInPlace() throws IOException {
    throw writeUnsupported();
  }

  @Override
  public void saveToStream(OutputStream out) throws IOException {
    throw writeUnsupported();
  }

    @Override
    public ExcelWorkbookReader getExcelWorkbookReader(File file) throws IOException, InterruptedException {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
