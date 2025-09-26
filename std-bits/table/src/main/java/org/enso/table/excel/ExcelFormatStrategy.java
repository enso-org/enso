package org.enso.table.excel;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;

import org.apache.poi.ss.usermodel.Workbook;

/**
 * Abstract base class for Excel format-specific operations that owns a {@link Workbook} instance.
 *
 * <p>Subclasses encapsulate how to open existing files, create new workbooks, persist data either
 * in-place or to an output stream, and perform any required cleanup (e.g. disposing temporary
 * files for streaming workbooks).
 */
public abstract class ExcelFormatStrategy {

  protected Workbook workbook;
  protected boolean preExistingFile;

  /** Returns the managed workbook instance (after {@link #openExisting} or {@link #createNew}). */
  public Workbook getWorkbook() {
    return workbook;
  }

  /** Opens an existing Excel file using the given access mode and sets {@link #workbook}. */
  public abstract void openExisting(File file, boolean writeAccess) throws IOException;

  /** Creates a new empty workbook for this format and sets {@link #workbook}. */
  public abstract void createNew();

  /** Saves changes to an existing file in place, if supported by the format. */
  public abstract void saveInPlace() throws IOException;

  /** Writes the current workbook to the provided output stream. */
  public abstract void saveToStream(OutputStream out) throws IOException;

  /** Performs any format-specific cleanup after saving/closing. Default is no-op. */
  public void cleanup() throws IOException {}

  /**
   * Opens a workbook for write. Detects whether the file already exists and is non-empty and
   * chooses between {@link #openExisting(File, boolean)} and {@link #createNew()} accordingly.
   */
  public void openForWrite(File file) throws IOException {
    this.preExistingFile = file.exists() && Files.exists(file.toPath()) && Files.size(file.toPath()) > 0;
    if (preExistingFile) {
      openExisting(file, true);
    } else {
      createNew();
    }
  }

  /**
   * Finalises a write by saving either in-place (for existing files) or to a newly created file
   * stream. Always calls {@link #cleanup()} afterwards.
   */
  public void finaliseWrite(File file) throws IOException {
    // Recompute in case this strategy was not the same instance used to open.
    boolean exists = file.exists() && Files.exists(file.toPath()) && Files.size(file.toPath()) > 0;
    if (exists) {
      saveInPlace();
    } else {
      try (OutputStream fileOut = Files.newOutputStream(file.toPath());
          BufferedOutputStream workbookOut = new BufferedOutputStream(fileOut)) {
        saveToStream(workbookOut);
      }
    }
    cleanup();
  }
}
