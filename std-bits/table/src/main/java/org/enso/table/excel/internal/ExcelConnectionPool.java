package org.enso.table.excel.internal;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;

import org.enso.base.cache.ReloadDetector;
import org.enso.table.excel.ExcelFileFormat;
import org.enso.table.excel.ExcelWorkbook;
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

  private ExcelConnectionPool() {}

  public synchronized <R> R performReadOnlyAction(
      File file,
      ExcelFileFormat format,
      FunctionWithException<ExcelWorkbook, R, InterruptedException> action)
      throws IOException, InterruptedException {
    ReloadDetector.clearOnReload(this);
    var workbook = openCachedConnection(file, format);
    return action.apply(workbook);
  }

  public synchronized void closeConnection(File file, ExcelFileFormat format) throws IOException {
    String key = getKeyForFile(file, format);
    ExcelWorkbook existingWorkbook = workbooksCache.get(key);
    if (existingWorkbook != null) {
      existingWorkbook.close();
      workbooksCache.remove(key);
    }
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

  private String getKeyForFile(File file, ExcelFileFormat format) throws IOException {
    String pathPart = file.getCanonicalPath();
    return pathPart + "::" + format.name();
  }
}
