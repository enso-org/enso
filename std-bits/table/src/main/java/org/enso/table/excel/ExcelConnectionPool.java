package org.enso.table.excel;

import org.apache.poi.UnsupportedFileFormatException;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.exceptions.OpenXML4JRuntimeException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.openxml4j.opc.PackageAccess;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.AccessMode;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.function.Function;

public class ExcelConnectionPool {
  public static final ExcelConnectionPool INSTANCE = new ExcelConnectionPool();

  private ExcelConnectionPool() {
  }

  public ReadOnlyExcelConnection openReadOnlyConnection(File file, ExcelFileFormat format) throws IOException {
    synchronized (this) {
      if (isCurrentlyWriting) {
        throw new IllegalStateException("Cannot open a read-only Excel connection while an Excel file is being " +
            "written to. This is a bug in the Table library.");
      }

      if (!file.exists()) {
        throw new FileNotFoundException(file.toString());
      }

      String key = getKeyForFile(file);
      ConnectionRecord existingRecord = records.get(key);
      if (existingRecord != null) {
        // Adapt the existing record
        if (existingRecord.format != format) {
          throw new ExcelFileFormatMismatchException("Requesting to open " + file + " as " + format + ", but it was " +
              "already opened as " + existingRecord.format + ".");
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

  public static class WriteHelper {
    private final ExcelFileFormat format;

    public WriteHelper(ExcelFileFormat format) {
      this.format = format;
    }

    public <R> R writeWorkbook(File file, Function<Workbook, R> writeAction) throws IOException {
      boolean preExistingFile = file.exists() && Files.size(file.toPath()) > 0;

      try (Workbook workbook = preExistingFile ? ExcelConnectionPool.openWorkbook(file, format, true) :
          createEmptyWorkbook(format)) {
        R result = writeAction.apply(workbook);

        if (preExistingFile) {
          // Save the file in place.
          switch (workbook) {
            case HSSFWorkbook wb -> {
              wb.write();
            }
            case XSSFWorkbook wb -> {
              try {
                wb.write(null);
              } catch (OpenXML4JRuntimeException e) {
                // Ignore: Workaround for bug https://bz.apache.org/bugzilla/show_bug.cgi?id=59252
              }
            }
            default -> throw new IllegalStateException("Unknown workbook type: " + workbook.getClass());
          }
        } else {
          try (OutputStream fileOut = Files.newOutputStream(file.toPath())) {
            try (BufferedOutputStream workbookOut = new BufferedOutputStream(fileOut)) {
              workbook.write(workbookOut);
            }
          }
        }

        return result;
      }
    }
  }

  /**
   * Executes a write action, ensuring that any other Excel connections are closed during the action, so that it can
   * modify the file. Any existing connections are re-opened after the operation finishes (regardless of its success or
   * error).
   * <p>
   * The action gets a {@link WriteHelper} object that can be used to open the workbook for reading or writing. The
   * action must take care to close that workbook before returning.
   * <p>
   * Additional files that should be closed during the write action can be specified in the {@code accompanyingFiles}
   * argument. These may be related temporary files that are written during the write operation and also need to get
   * 'unlocked' for the time of write.
   */
  public <R> R lockForWriting(File file, ExcelFileFormat format, File[] accompanyingFiles,
                              Function<WriteHelper, R> action) throws IOException {
    synchronized (this) {
      if (isCurrentlyWriting) {
        throw new IllegalStateException("Another Excel write is in progress on the same thread. This is a bug in the " +
            "Table library.");
      }

      isCurrentlyWriting = true;
      try {
        String key = getKeyForFile(file);
        ArrayList<ConnectionRecord> recordsToReopen = new ArrayList<>(1 + accompanyingFiles.length);

        try {
          // Close the existing connection, if any - to avoid the write operation failing due to the file being locked.
          ConnectionRecord existingRecord = records.get(key);
          if (existingRecord != null) {
            existingRecord.close();
            recordsToReopen.add(existingRecord);
          }

          verifyIsWritable(file);

          for (File accompanyingFile : accompanyingFiles) {
            String accompanyingKey = getKeyForFile(accompanyingFile);
            ConnectionRecord accompanyingRecord = records.get(accompanyingKey);
            if (accompanyingRecord != null) {
              accompanyingRecord.close();
              recordsToReopen.add(accompanyingRecord);
            }

            verifyIsWritable(accompanyingFile);
          }

          WriteHelper helper = new WriteHelper(format);
          return action.apply(helper);
        } finally {
          // Reopen the closed connections
          for (ConnectionRecord record : recordsToReopen) {
            record.reopen(false);
          }
        }

      } finally {
        isCurrentlyWriting = false;
      }
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

  private static Workbook createEmptyWorkbook(ExcelFileFormat format) {
    return switch (format) {
      case XLS -> new HSSFWorkbook();
      case XLSX -> new XSSFWorkbook();
    };
  }

  public static class ExcelFileFormatMismatchException extends UnsupportedFileFormatException {
    public ExcelFileFormatMismatchException(String message) {
      super(message);
    }
  }
}
