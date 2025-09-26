package org.enso.table.excel;

import java.io.File;
import java.io.IOException;
import java.nio.file.AccessMode;
import java.nio.file.Files;
import java.nio.file.Path;
import org.apache.poi.ss.usermodel.Workbook;

public class ExcelWriteHelper {
  private final File file;
  private final ExcelFileFormat format;

  public ExcelWriteHelper(File file, ExcelFileFormat format) {
    this.file = file;
    this.format = format;
  }

  public static Workbook openWorkbookForWrite(File file, ExcelFileFormat format) throws IOException {
    verifyIsWritable(file);
    ExcelFormatStrategy strategy = format.createStrategy();
    strategy.openForWrite(file);
    return strategy.getWorkbook();
  }

  private static void verifyIsWritable(File file) throws IOException {
    Path path = file.toPath();

    if (!Files.exists(path)) {
      // If the file does not exist, we assume that we can create it.
      return;
    }

    path.getFileSystem().provider().checkAccess(path, AccessMode.WRITE, AccessMode.READ);
  }

  public static void finaliseWorkbookWrite(File file, ExcelFileFormat format, Workbook workbook)
      throws IOException {
    ExcelFormatStrategy strategy = format.createStrategy();
    strategy.workbook = workbook;
    strategy.finaliseWrite(file);
  }

  public static Workbook openWorkbook(File file, ExcelFileFormat format, boolean writeAccess)
      throws IOException {
    ExcelFormatStrategy strategy = format.createStrategy();
    strategy.openExisting(file, writeAccess);
    return strategy.getWorkbook();
  }
}
