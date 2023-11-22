package org.enso.table.read.excel;

import org.apache.poi.ss.usermodel.Workbook;

import java.io.IOException;
import java.util.function.Function;

public class ExcelConnection implements AutoCloseable {

  private final ExcelConnectionPool myPool;
  final String key;
  final ExcelConnectionPool.ConnectionRecord record;

  ExcelConnection(ExcelConnectionPool myPool, String key, ExcelConnectionPool.ConnectionRecord record) {
    this.myPool = myPool;
    this.key = key;
    this.record = record;
  }

  @Override
  public void close() throws IOException {
    myPool.release(this);
  }

  public <T> T withWorkbook(Function<Workbook, T> f) throws IOException {
    return record.withWorkbook(f);
  }
}
