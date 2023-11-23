package org.enso.table.excel;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.openxml4j.opc.PackageAccess;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.enso.table.write.ExistingFileBehavior;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.function.Function;

public class ExcelConnectionPool {
  public static final ExcelConnectionPool INSTANCE = new ExcelConnectionPool();

  private ExcelConnectionPool() {
  }

  public ReadOnlyExcelConnection openReadOnlyConnection(File file, ExcelFileFormat format) throws IOException {
    synchronized (this) {
      if (isCurrentlyWriting) {
        throw new IllegalStateException("Cannot open a read-only Excel connection while an Excel file is being written to. This is a bug in the Table library.");
      }

      if (!file.exists()) {
        throw new FileNotFoundException(file.toString());
      }

      String key = getKeyForFile(file);
      ConnectionRecord existingRecord = records.get(key);
      if (existingRecord != null) {
        // Adapt the existing record
        if (existingRecord.format != format) {
          throw new IllegalStateException("Requesting to open " + file + " as " + format + ", but it was already " +
              "opened as " + existingRecord.format);
        }

        existingRecord.refCount++;

        return new ReadOnlyExcelConnection(this, key, existingRecord);
      } else {
        // Create the new record
        ConnectionRecord record = new ConnectionRecord();
        record.refCount = 1;
        record.file = file;
        record.format = format;
        record.workbook = openWorkbook(file, format, false);
        records.put(key, record);
        return new ReadOnlyExcelConnection(this, key, record);
      }
    }
  }

  public <R> R performWrite(File file, ExcelFileFormat format, ExistingFileBehavior existingFileBehavior, Function<Workbook, R> writeAction) throws IOException {
    synchronized (this) {
      if (isCurrentlyWriting) {
        throw new IllegalStateException("Another Excel write is in progress on the same thread. This is a bug in the Table library.");
      }

      isCurrentlyWriting = true;
      try {
        String key = getKeyForFile(file);

        ConnectionRecord existingRecord = records.get(key);
        // Close the existing read-only connection, if any.
        if (existingRecord != null) {
          existingRecord.close();
        }

        try {

          // TODO backup logic
          try (Workbook workbook = openWorkbook(file, format, true)) {
            return writeAction.apply(workbook);
          }

        } finally {
          // Reopen the read-only connection.
          if (existingRecord != null) {
            existingRecord.reopen(false);
          }
        }

      } finally {
        isCurrentlyWriting = false;
      }
    }
  }

  private String getKeyForFile(File file) throws IOException {
    return file.getCanonicalPath();
  }

  void release(ReadOnlyExcelConnection excelConnection) throws IOException {
    synchronized (this) {
      excelConnection.record.refCount--;
      if (excelConnection.record.refCount <= 0) {
        excelConnection.record.close();
        records.remove(excelConnection.key);
      }
    }
  }

  private final HashMap<String, ConnectionRecord> records = new HashMap<>();
  private boolean isCurrentlyWriting = false;

  static class ConnectionRecord {
    private int refCount;
    private File file;
    private ExcelFileFormat format;
    private Workbook workbook;
    private IOException initializationException = null;

    <T> T withWorkbook(Function<Workbook, T> action) throws IOException {
      synchronized (this) {
        return action.apply(accessCurrentWorkbook());
      }
    }

    public void close() throws IOException {
      synchronized (this) {
        if (workbook != null) {
          workbook.close();
        }

        workbook = null;
      }
    }

    void reopen(boolean throwOnFailure) throws IOException {
      synchronized (this) {
        if (workbook != null) {
          throw new IllegalStateException("The workbook is already open.");
        }

        try {
          workbook = openWorkbook(file, format, false);
        } catch (IOException e) {
          initializationException = e;
          if (throwOnFailure) {
            throw e;
          }
        }
      }
    }

    private Workbook accessCurrentWorkbook() throws IOException {
      synchronized (this) {
        if (workbook == null) {
          if (initializationException != null) {
            throw initializationException;
          } else {
            throw new IllegalStateException("The workbook is used after being closed.");
          }
        }

        return workbook;
      }
    }
  }

  private static Workbook openWorkbook(File file, ExcelFileFormat format, boolean writeAccess) throws IOException {
    return switch (format) {
      case XLS -> {
        boolean readOnly = !writeAccess;
        POIFSFileSystem fs = new POIFSFileSystem(file, readOnly);
        try {
          // If the initialization succeeds, the POIFSFileSystem will be closed by the HSSFWorkbook::close.
          yield new HSSFWorkbook(fs);
        } catch (Exception e) {
          fs.close();
          throw e;
        }
      }
      case XLSX -> {
        try {
          PackageAccess access = writeAccess ? PackageAccess.READ_WRITE : PackageAccess.READ;
          OPCPackage pkg = OPCPackage.open(file, access);
          try {
            yield new XSSFWorkbook(pkg);
          } catch (IOException e) {
            pkg.close();
            throw e;
          }
        } catch (InvalidFormatException e) {
          throw new IOException("Invalid format encountered when opening the file " + file + " as " + format + ".", e);
        }
      }
    };
  }
}
