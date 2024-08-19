package org.enso.table.excel;

import java.io.IOException;
import java.util.function.Function;

public class ReadOnlyExcelConnection implements AutoCloseable {

  private final ExcelConnectionPool myPool;
  final String key;
  ExcelConnectionPool.ConnectionRecord record;

  ReadOnlyExcelConnection(
      ExcelConnectionPool myPool, String key, ExcelConnectionPool.ConnectionRecord record) {
    this.myPool = myPool;
    this.key = key;
    this.record = record;
  }

  @Override
  public synchronized void close() throws IOException {
    if (record == null) {
      // already closed
      return;
    }

    myPool.release(this);
    record = null;
  }

  public synchronized <T> T withWorkbook(Function<ExcelWorkbook, T> f) throws IOException {
    if (record == null) {
      throw new IllegalStateException("ReadOnlyExcelConnection is being used after it was closed.");
    }

    return record.withWorkbook(p -> f.apply(ExcelWorkbook.ForWorkbook(p)));
  }
}
