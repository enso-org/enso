package org.enso.table.excel;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

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
}
