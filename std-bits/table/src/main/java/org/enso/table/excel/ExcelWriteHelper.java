package org.enso.table.excel;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.util.function.Function;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.exceptions.OLE2NotOfficeXmlFileException;
import org.apache.poi.openxml4j.exceptions.OpenXML4JRuntimeException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.openxml4j.opc.PackageAccess;
import org.apache.poi.poifs.filesystem.OfficeXmlFileException;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

public class ExcelWriteHelper {
  private final File file;
  private final ExcelFileFormat format;

  public ExcelWriteHelper(File file, ExcelFileFormat format) {
    this.file = file;
    this.format = format;
  }

  public <R> R writeWorkbook(Function<Workbook, R> writeAction) throws IOException {
    boolean preExistingFile = file.exists() && Files.size(file.toPath()) > 0;

    try (Workbook workbook =
        preExistingFile ? openWorkbook(file, format, true) : createEmptyWorkbook(format)) {
      R result = writeAction.apply(workbook);

      if (preExistingFile) {
        // Save the file in place.
        switch (workbook) {
          case HSSFWorkbook wb -> {
            wb.write();
          }
          case XSSFWorkbook wb -> {
            try {
              wb.write(null);
            } catch (OpenXML4JRuntimeException e) {
              // Ignore: Workaround for bug https://bz.apache.org/bugzilla/show_bug.cgi?id=59252
            }
          }
          default ->
              throw new IllegalStateException("Unknown workbook type: " + workbook.getClass());
        }
      } else {
        try (OutputStream fileOut = Files.newOutputStream(file.toPath())) {
          try (BufferedOutputStream workbookOut = new BufferedOutputStream(fileOut)) {
            workbook.write(workbookOut);
          }
        }
      }

      // If we used the streaming workbook, ensure temp files are deleted.
      if (workbook instanceof SXSSFWorkbook sxssf) {
        sxssf.dispose();
      }

      return result;
    }
  }

  public static Workbook openWorkbook(File file, ExcelFileFormat format, boolean writeAccess)
      throws IOException {
    return switch (format) {
      case XLS -> {
        try {
          boolean readOnly = !writeAccess;
          POIFSFileSystem fs = new POIFSFileSystem(file, readOnly);
          try {
            // If the initialization succeeds, the POIFSFileSystem will be closed by the
            // HSSFWorkbook::close.
            yield new HSSFWorkbook(fs);
          } catch (IOException e) {
            fs.close();
            throw e;
          }
        } catch (OfficeXmlFileException | OLE2NotOfficeXmlFileException e) {
          throw new IOException(
              "Invalid format encountered when opening the file " + file + " as " + format + ".",
              e);
        }
      }
      case XLSX, XLSX_FALLBACK -> {
        try {
          PackageAccess access = writeAccess ? PackageAccess.READ_WRITE : PackageAccess.READ;
          OPCPackage pkg = OPCPackage.open(file, access);
          try {
            yield new XSSFWorkbook(pkg);
          } catch (IOException e) {
            pkg.close();
            throw e;
          }
        } catch (InvalidFormatException | OLE2NotOfficeXmlFileException e) {
          throw new IOException(
              "Invalid format encountered when opening the file " + file + " as " + format + ".",
              e);
        }
      }
    };
  }

  private static Workbook createEmptyWorkbook(ExcelFileFormat format) {
    return switch (format) {
      case XLS -> new HSSFWorkbook();
      case XLSX, XLSX_FALLBACK -> new SXSSFWorkbook();
    };
  }
}
