package org.enso.table.excel;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.AccessMode;
import java.nio.file.Files;
import java.nio.file.Path;
import org.apache.poi.ss.usermodel.Workbook;

/**
 * Abstract base class for Excel format-specific operations that owns a {@link Workbook} instance.
 *
 * <p>Subclasses encapsulate how to open existing files, create new workbooks, persist data either
 * in-place or to an output stream, and perform any required cleanup (e.g. disposing temporary files
 * for streaming workbooks).
 */
public abstract class ExcelFormatStrategy {

  protected Workbook workbook;
  protected boolean preExistingFile;
  protected File file;

  /**
   * Creates a concrete strategy instance for the given Excel file format.
   *
   * @param format The format for the excel File
   * @return The stratgey for the given option
   */
  public static ExcelFormatStrategy createStrategy(ExcelFileFormat format) {
    switch (format) {
      case XLS -> {
        return new XlsFormatStrategy();
      }
      case XLSB -> {
        return new XlsbFormatStrategy();
      }
      case XLSX, XLSX_FALLBACK -> {
        return new XlsxFormatStrategy();
      }
      default -> throw new AssertionError();
    }
  }

  /** Opens an existing Excel file using the given access mode and sets {@link #workbook}. */
  public abstract Workbook openExisting(File file, boolean writeAccess) throws IOException;

  /** Returns a reader for the current workbook. */
  public abstract ExcelWorkbookReader getExcelWorkbookReader(File file)
      throws IOException, InterruptedException;

  /** Creates a new empty workbook for this format and sets {@link #workbook}. */
  public abstract Workbook createNew();

  /** Saves changes to an existing file in place, if supported by the format. */
  public abstract void saveInPlace() throws IOException;

  /** Writes the current workbook to the provided output stream. */
  public abstract void saveToStream(OutputStream out) throws IOException;

  /** Performs any format-specific cleanup after saving/closing. Default is no-op. */
  public void cleanup() throws IOException {}

  /**
   * Opens a workbook for write. Detects whether the file already exists and is non-empty and
   * chooses between {@link #openExisting(File, boolean)} and {@link #createNew()} accordingly.
   *
   * @return
   */
  public Workbook openForWrite(File file) throws IOException {
    verifyIsWritable(file);
    this.file = file;
    this.preExistingFile =
        file.exists() && Files.exists(file.toPath()) && Files.size(file.toPath()) > 0;
    if (preExistingFile) {
      openExisting(file, true);
    } else {
      createNew();
    }
    return workbook;
  }

  /**
   * Finalises a write by saving either in-place (for existing files) or to a newly created file
   * stream. Always calls {@link #cleanup()} afterwards.
   */
  public void finaliseWrite() throws IOException {
    if (preExistingFile) {
      saveInPlace();
    } else {
      try (OutputStream fileOut = Files.newOutputStream(file.toPath());
          BufferedOutputStream workbookOut = new BufferedOutputStream(fileOut)) {
        saveToStream(workbookOut);
      }
    }
    cleanup();
  }

  private static void verifyIsWritable(File file) throws IOException {
    Path path = file.toPath();

    if (!Files.exists(path)) {
      // If the file does not exist, we assume that we can create it.
      return;
    }

    path.getFileSystem().provider().checkAccess(path, AccessMode.WRITE, AccessMode.READ);
  }
}
