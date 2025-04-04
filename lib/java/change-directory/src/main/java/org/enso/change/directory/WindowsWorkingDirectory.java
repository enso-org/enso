package org.enso.change.directory;

import java.util.List;
import org.enso.common.Platform;
import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@CContext(WindowsWorkingDirectory.Directives.class)
public final class WindowsWorkingDirectory extends WorkingDirectory {
  static final WindowsWorkingDirectory INSTANCE = new WindowsWorkingDirectory();
  private static final Logger LOGGER = LoggerFactory.getLogger(WindowsWorkingDirectory.class);

  @Override
  public String currentWorkingDir() {
    byte[] buf = new byte[4096];
    String path;
    try (var ptrHolder = CTypeConversion.toCBytes(buf)) {
      var ptr = ptrHolder.get();
      var ret = GetCurrentDirectoryA(4096, ptr);
      if (ret == 0) {
        LOGGER.error("GetCurrentDirectory failed with {}", ret);
        return null;
      } else {
        path = new String(buf);
      }
    }
    return path.trim();
  }

  @Override
  public boolean changeWorkingDir(String path) {
    try (var cPath = CTypeConversion.toCString(path + "\0")) {
      var res = SetCurrentDirectoryA(cPath.get());
      if (res == 0) {
        LOGGER.error("SetCurrrentDirectory to {} failed with {}", path, res);
        return false;
      }
      return true;
    } catch (Throwable t) {
      LOGGER.error("Cannot change working directory to " + path + " on Windows", t);
      throw t;
    }
  }

  @Override
  public boolean existsImpl(String path) {
    try (var cPath = CTypeConversion.toCString(path)) {
      var res = PathFileExistsA(cPath.get());
      return res != 0;
    } catch (Throwable t) {
      LOGGER.error("Cannot check if {} exists on Windows", path, t);
      return false;
    }
  }

  @Override
  public boolean isDirectory(String path) {
    try (var cPath = CTypeConversion.toCString(path)) {
      var res = PathIsDirectoryA(cPath.get());
      return res != 0;
    } catch (Throwable t) {
      LOGGER.error("Cannot check if {} is a directory on Windows", path, t);
      return false;
    }
  }

  /**
   * <a
   * href="https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-getcurrentdirectory">Official
   * docs</a>
   */
  @CFunction
  static native int GetCurrentDirectoryA(int nBufferLength, CCharPointer lpBuffer);

  /**
   * <a
   * href="https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-setcurrentdirectory">Official
   * docs</a>
   */
  @CFunction
  static native int SetCurrentDirectoryA(CCharPointer lpPathName);

  /**
   * <a
   * href="https://learn.microsoft.com/en-us/windows/win32/api/shlwapi/nf-shlwapi-pathfileexistsa">Official
   * docs</a>
   */
  @CFunction
  static native int PathFileExistsA(CCharPointer pszPath);

  @CFunction
  static native int PathIsDirectoryA(CCharPointer pszPath);

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
