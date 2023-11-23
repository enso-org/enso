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

  public <T> T withWorkbookCommit(Function<Workbook, T> f) throws IOException {
    try {
      return record.withWorkbook(workbook -> {
        T result = f.apply(workbook);
        try {
          commitWorkbookChanges(workbook);
        } catch (IOException e) {
          throw new WrappedIOException(e);
        }
        return result;
      });
    } catch (WrappedIOException e) {
      throw e.cause;
    }
  }

  private static class WrappedIOException extends RuntimeException {
    final IOException cause;

    public WrappedIOException(IOException e) {
      super(e);
      this.cause = e;
    }
  }
}
