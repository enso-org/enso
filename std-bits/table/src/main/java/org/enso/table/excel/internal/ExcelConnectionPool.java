package org.enso.table.excel.internal;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import org.enso.base.cache.ReloadDetector;
import org.enso.table.excel.ExcelFileFormat;
import org.enso.table.excel.ExcelFormatStrategy;
import org.enso.table.excel.ExcelWorkbookReader;
import org.enso.table.util.FunctionWithException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A singleton cache for {@link ExcelWorkbookReader} connections.
 *
 * <p>Provides read-only access helpers that reuse an open workbook connection per excel workbook.
 * Integrates with {@link ReloadDetector} to clear state on reload and allows explicit closing of
 * cached connections.
 */
public class ExcelConnectionPool implements ReloadDetector.HasClearableCache {
  public static final ExcelConnectionPool INSTANCE = new ExcelConnectionPool();

  static {
    // Register after construction to avoid leaking `this` from the constructor.
    ReloadDetector.register(INSTANCE);
  }

  private static final Logger LOGGER = LoggerFactory.getLogger(ExcelConnectionPool.class);

  private final HashMap<String, ExcelWorkbookReader> workbooksCache = new HashMap<>();

  /** Private constructor to enforce the singleton pattern. Use {@link #INSTANCE}. */
  private ExcelConnectionPool() {}

  /**
   * Performs a read-only action using a cached {@link ExcelWorkbookReader} connection.
   *
   * <p>Registers this pool with {@link ReloadDetector} so the cache is cleared on reload, opens (or
   * reuses) a cached workbook for the given file and format, and applies the provided action. The
   * action is allowed to throw {@link InterruptedException}.
   *
   * @param file the Excel workbook file to open
   * @param format the expected {@link ExcelFileFormat}
   * @param action a function operating on the opened {@link ExcelWorkbookReader}
   * @param <R> the action's return type
   * @return the result produced by {@code action}
   * @throws IOException if the file cannot be opened or resolved
   * @throws InterruptedException if the action indicates interruption
   */
  public <R> R performReadOnlyAction(
      File file,
      ExcelFileFormat format,
      FunctionWithException<ExcelWorkbookReader, R, InterruptedException> action)
      throws IOException, InterruptedException {
    ReloadDetector.clearOnReload(this);
    var workbook = openCachedConnection(file, format);
    return action.apply(workbook);
  }

  /**
   * Closes and evicts a cached workbook connection for the specified file and format.
   *
   * <p>If no cached connection exists, the call is a no-op.
   *
   * @param file the Excel file whose connection should be closed
   * @param format the {@link ExcelFileFormat} used to cache the connection
   * @throws IOException if closing the underlying workbook fails
   */
  public void closeConnection(File file, ExcelFileFormat format) throws IOException {
    String key = getKeyForFile(file, format);
    ExcelWorkbookReader existingWorkbook = workbooksCache.get(key);
    if (existingWorkbook != null) {
      existingWorkbook.close();
      workbooksCache.remove(key);
    }
  }

  /**
   * Clears and closes all cached workbook connections.
   *
   * <p>Invoked when a reload occurs to ensure no stale resources remain open. Any IO errors during
   * close are logged and suppressed.
   */
  @Override
  public void clearCache() {
    for (var record : workbooksCache.values()) {
      try {
        record.close();
      } catch (IOException e) {
        LOGGER.error("Unable to close " + record, e);
      }
    }
    workbooksCache.clear();
  }

  /**
   * Returns the number of cached workbook connections.
   *
   * <p>Public for testing and diagnostics.
   *
   * @return count of cached workbooks
   */
  public int getWorkbooksCacheSize() {
    return workbooksCache.size();
  }

  /**
   * Returns a cached {@link ExcelWorkbookReader} for the given file and format, opening it if
   * necessary.
   *
   * <p>Validates that the file exists, then uses a canonical-path-based key to locate or create a
   * cached connection.
   *
   * @param file the Excel file to access
   * @param format the {@link ExcelFileFormat} of the file
   * @return an open {@link ExcelWorkbookReader}
   * @throws FileNotFoundException if {@code file} does not exist
   * @throws IOException if the file cannot be opened or canonicalized
   * @throws InterruptedException if opening the workbook is interrupted
   */
  private ExcelWorkbookReader openCachedConnection(File file, ExcelFileFormat format)
      throws IOException, InterruptedException {
    if (!file.exists()) {
      throw new FileNotFoundException(file.toString());
    }
    String key = getKeyForFile(file, format);
    var workbook = workbooksCache.get(key);
    if (workbook == null) {
      var strategy = ExcelFormatStrategy.createStrategy(format);
      workbook = strategy.getExcelWorkbookReader(file);
      workbooksCache.put(key, workbook);
    }
    return workbook;
  }

  /**
   * Builds a stable cache key for a file and format using the canonical file path.
   *
   * @param file the Excel file
   * @param format the {@link ExcelFileFormat}
   * @return a unique key combining canonical path and format
   * @throws IOException if the canonical path cannot be resolved
   */
  private String getKeyForFile(File file, ExcelFileFormat format) throws IOException {
    String pathPart = file.getCanonicalPath();
    return pathPart + "::" + format.name();
  }
}
