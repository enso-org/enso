package org.enso.table.excel.internal;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.AccessMode;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.function.Function;

import org.enso.base.cache.ReloadDetector;
import org.enso.table.excel.ExcelFileFormat;
import org.enso.table.excel.ExcelWorkbook;
import org.enso.table.excel.ExcelWriteHelper;
import org.enso.table.util.FunctionWithException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ExcelConnectionPool implements ReloadDetector.HasClearableCache {
  public static final ExcelConnectionPool INSTANCE = new ExcelConnectionPool();

  static {
    // Register after construction to avoid leaking `this` from the constructor.
    ReloadDetector.register(INSTANCE);
  }

  private static final Logger LOGGER = LoggerFactory.getLogger(ExcelConnectionPool.class);

  private final HashMap<String, ExcelWorkbook> workbooksCache = new HashMap<>();
  private boolean isCurrentlyWriting = false;

  private ExcelConnectionPool() {}

  public synchronized <R> R performReadOnlyAction(
      File file,
      ExcelFileFormat format,
      FunctionWithException<ExcelWorkbook, R, InterruptedException> action)
      throws IOException, InterruptedException {
    if (isCurrentlyWriting) {
      throw new IllegalStateException(
          "Cannot open a read-only Excel connection while an Excel file is being "
              + "written to. This is a bug in the Table library.");
    }
    ReloadDetector.clearOnReload(this);
    var workbook = openCachedConnection(file, format);
    return action.apply(workbook);
  }

  /**
   * Executes a write action, ensuring that any other Excel connections are closed during the
   * action, so that it can modify the file. Any existing connections are re-opened after the
   * operation finishes (regardless of its success or error).
   *
   * <p>The action gets a {@link WriteHelper} object that can be used to open the workbook for
   * reading or writing. The action must take care to close that workbook before returning.
   *
   * <p>Additional files that should be closed during the write action can be specified in the
   * {@code accompanyingFiles} argument. These may be related temporary files that are written
   * during the write operation and also need to get 'unlocked' for the time of write.
   */
  public synchronized <R> R performWriteAction(
      File file,
      ExcelFileFormat format,
      Function<ExcelWriteHelper, R> action)
      throws IOException, InterruptedException {
    if (isCurrentlyWriting) {
      throw new IllegalStateException(
          "Another Excel write is in progress on the same thread. This is a bug in the "
              + "Table library.");
    }
    isCurrentlyWriting = true;
    try {
      // Close the existing connection, if any - to avoid the write operation failing due to the
      // file being locked.
      closeCachedConnection(file, format);
      verifyIsWritable(file);

      ExcelWriteHelper helper = new ExcelWriteHelper(file, format);
      return action.apply(helper);
    } finally {
      isCurrentlyWriting = false;
    }
  }

  public synchronized void closeConnection(File file, ExcelFileFormat format) throws IOException {
    if (isCurrentlyWriting) {
      throw new IllegalStateException(
          "Cannot close a Excel connection while an Excel file is being "
              + "written to. This is a bug in the Table library.");
    }
    closeCachedConnection(file, format);
  }

  /** If a reload has just happened, clear the ConnectionRecord cache. */
  @Override
  public synchronized void clearCache() {
    for (var record : workbooksCache.values()) {
      try {
        record.close();
      } catch (IOException e) {
        LOGGER.error("Unable to close " + record, e);
      }
    }
    workbooksCache.clear();
  }

  /** Public for testing. */
  public synchronized int getConnectionRecordCount() {
    return workbooksCache.size();
  }

  private ExcelWorkbook openCachedConnection(File file, ExcelFileFormat format)
      throws IOException, InterruptedException {
    if (!file.exists()) {
      throw new FileNotFoundException(file.toString());
    }
    String key = getKeyForFile(file, format);
    var workbook = workbooksCache.get(key);
    if (workbook == null) {
      workbook = ExcelWorkbook.getExcelWorkbook(file, format);
      workbooksCache.put(key, workbook);
    }
    return workbook;
  }

  private void closeCachedConnection(File file, ExcelFileFormat format) throws IOException {
    String key = getKeyForFile(file, format);
    ExcelWorkbook existingWorkbook = workbooksCache.get(key);
    if (existingWorkbook != null) {
      existingWorkbook.close();
      workbooksCache.remove(key);
    }
  }

  private void verifyIsWritable(File file) throws IOException {
    Path path = file.toPath();

    if (!Files.exists(path)) {
      // If the file does not exist, we assume that we can create it.
      return;
    }

    path.getFileSystem().provider().checkAccess(path, AccessMode.WRITE, AccessMode.READ);
  }

  private String getKeyForFile(File file, ExcelFileFormat format) throws IOException {
    String pathPart = file.getCanonicalPath();
    return pathPart + "::" + format.name();
  }
}
