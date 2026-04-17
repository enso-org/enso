package org.enso.os.environment.chdir;

import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.util.List;
import org.enso.common.Platform;
import org.graalvm.nativeimage.UnmanagedMemory;
import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.graalvm.word.PointerBase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@CContext(WindowsWorkingDirectory.Directives.class)
final class WindowsWorkingDirectory extends WorkingDirectory {

  static final WindowsWorkingDirectory INSTANCE = new WindowsWorkingDirectory();
  private static final Logger LOGGER = LoggerFactory.getLogger(WindowsWorkingDirectory.class);

  // WChars in windows are 2 bytes
  private static final int WCHAR_SIZE = 2;

  // Windows MAX_PATH is 260 and MAX_PATH_WIDE is 32767
  private static final int MAX_LENGTH = 32767;

  private static String wcharPtrAsString(PointerBase buffer, int length) {
    return CTypeConversion.asByteBuffer(buffer, length * WCHAR_SIZE)
        .order(ByteOrder.LITTLE_ENDIAN)
        .asCharBuffer()
        .toString();
  }

  private static PointerBase stringAsWCharPtr(String input) {
    var bytes = input.getBytes(StandardCharsets.UTF_16LE);
    var buffer = UnmanagedMemory.malloc(bytes.length + 2);
    CTypeConversion.asByteBuffer(buffer, bytes.length + 2)
        .order(ByteOrder.LITTLE_ENDIAN)
        .put(bytes)
        .put(new byte[] {0, 0});
    return buffer;
  }

  @Override
  public String currentWorkingDir() {
    var buffer = UnmanagedMemory.malloc(MAX_LENGTH * WCHAR_SIZE);
    try {
      int length = GetCurrentDirectoryW(MAX_LENGTH, buffer);
      if (length == 0 || length == MAX_LENGTH) {
        LOGGER.error("GetCurrentDirectory failed with length {}", length);
        return null;
      }

      var result = wcharPtrAsString(buffer, length);
      LOGGER.debug("Current working directory is {}", result);
      return result;
    } finally {
      UnmanagedMemory.free(buffer);
    }
  }

  @Override
  public boolean changeWorkingDir(String path) {
    path = normalizeSlashes(path);

    var buffer = stringAsWCharPtr(path);
    try {
      var res = SetCurrentDirectoryW(buffer);
      if (res == 0) {
        LOGGER.error("SetCurrrentDirectory to {} failed with {}", path, res);
        return false;
      }
      return true;
    } catch (Throwable t) {
      LOGGER.error("Cannot change working directory to " + path + " on Windows", t);
      throw t;
    } finally {
      UnmanagedMemory.free(buffer);
    }
  }

  @Override
  public boolean exists(String dir, String file) {
    dir = normalizeSlashes(dir);
    file = normalizeSlashes(file);
    var full = dir + Platform.separatorChar() + file;
    var buffer = stringAsWCharPtr(full);
    try {
      var res = PathFileExistsW(buffer);
      return res != 0;
    } catch (Throwable t) {
      LOGGER.error("Cannot check if {} exists on Windows", full, t);
      return false;
    } finally {
      UnmanagedMemory.free(buffer);
    }
  }

  private static String normalizeSlashes(String path) {
    var newPath = path.replace('/', Platform.separatorChar());
    if (newPath.endsWith("" + Platform.separatorChar())) {
      return newPath.substring(0, newPath.length() - 1);
    } else {
      return newPath;
    }
  }

  /**
   * <a
   * href="https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-getcurrentdirectory">Official
   * docs</a>
   */
  @CFunction
  static native int GetCurrentDirectoryW(int nBufferLength, PointerBase lpBuffer);

  /**
   * <a
   * href="https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-setcurrentdirectory">Official
   * docs</a>
   */
  @CFunction
  static native int SetCurrentDirectoryW(PointerBase lpPathName);

  /**
   * <a
   * href="https://learn.microsoft.com/en-us/windows/win32/api/shlwapi/nf-shlwapi-pathfileexistsa">Official
   * docs</a>
   */
  @CFunction
  static native int PathFileExistsW(PointerBase pszPath);

  static final class Directives implements CContext.Directives {
    @Override
    public boolean isInConfiguration() {
      return Platform.getOperatingSystem().isWindows();
    }

    @Override
    public List<String> getHeaderFiles() {
      return List.of("<windows.h>", "<shlwapi.h>");
    }

    @Override
    public List<String> getLibraries() {
      return List.of("Kernel32", "Shlwapi");
    }
  }
}
