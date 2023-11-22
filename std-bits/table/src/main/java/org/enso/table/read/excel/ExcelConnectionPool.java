package org.enso.table.read.excel;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.openxml4j.opc.PackageAccess;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.function.Function;

public class ExcelConnectionPool {
  public static final ExcelConnectionPool INSTANCE = new ExcelConnectionPool();

  private ExcelConnectionPool() {
  }

  public ExcelConnection openConnection(File file, ExcelFileFormat format, boolean write_access) throws IOException {
    synchronized (this) {
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

        if (write_access) {
          existingRecord.ensureWriteAccess();
        }

        return new ExcelConnection(this, key, existingRecord);
      } else {
        // Create the new record
        ConnectionRecord record = new ConnectionRecord();
        record.refCount = 1;
        record.hasWriteAccess = write_access;
        record.file = file;
        record.format = format;
        record.workbook = openWorkbook(file, format, write_access);
        records.put(key, record);
        return new ExcelConnection(this, key, record);
      }
    }
  }

  private String getKeyForFile(File file) throws IOException {
    return file.getCanonicalPath();
  }

  void release(ExcelConnection excelConnection) throws IOException {
    synchronized (this) {
      excelConnection.record.refCount--;
      if (excelConnection.record.refCount <= 0) {
        excelConnection.record.close();
        records.remove(excelConnection.key);
      }
    }
  }

  private final HashMap<String, ConnectionRecord> records = new HashMap<>();

  static class ConnectionRecord {
    private int refCount;
    private boolean hasWriteAccess;
    private File file;
    private ExcelFileFormat format;
    private Workbook workbook;
    private IOException initializationException = null;

    <T> T withWorkbook(Function<Workbook, T> action) throws IOException {
      synchronized (this) {
        return action.apply(accessCurrentWorkbook());
      }
    }

    private void ensureWriteAccess() throws IOException {
      synchronized (this) {
        if (hasWriteAccess) {
          return;
        }

        workbook.close();
        workbook = null;
        try {
          workbook = openWorkbook(file, format, true);
        } catch (IOException e) {
          throw recoverFromWriteAccessFailure(e);
        }

        hasWriteAccess = true;
      }
    }

    private IOException recoverFromWriteAccessFailure(IOException originalException) throws IOException {
      // If opening the workbook for writing has failed, it may mean we do not have the write permissions.
      // But we have already closed the old workbook - now, even other users cannot use it anymore.
      // To try recovery - we try reopening again in read mode - to ensure that at least the other users of the workbook can continue using it uninterrupted.
      try {
        workbook = openWorkbook(file, format, false);

        // Afterwards, we rethrow the exception anyway - because the current user did not get the requested write access.
        // But at least we kept other users happy.
        throw originalException;
      } catch (IOException e) {
        // We failed to reopen the workbook in read mode either - that means that other users will now be affected too. There is nothing more we can do.
        initializationException = e;
        throw originalException;
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
