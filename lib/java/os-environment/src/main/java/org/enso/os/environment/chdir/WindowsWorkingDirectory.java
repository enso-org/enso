package org.enso.os.environment.chdir;

import java.util.List;
import org.enso.common.Platform;
import org.graalvm.nativeimage.UnmanagedMemory;
import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.type.CShortPointer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@CContext(WindowsWorkingDirectory.Directives.class)
final class WindowsWorkingDirectory extends WorkingDirectory {
  static final WindowsWorkingDirectory INSTANCE = new WindowsWorkingDirectory();
  private static final Logger LOGGER = LoggerFactory.getLogger(WindowsWorkingDirectory.class);

  @Override
  public String currentWorkingDir() {
    var bufferLength = 4096;
    CShortPointer ptr = UnmanagedMemory.calloc(bufferLength * Character.BYTES);
    if (ptr == null) {
      LOGGER.error("Unable to allocate memory for current working directory.");
      return null;
    }
    try {
      var ret = GetCurrentDirectoryW(bufferLength, ptr);
      if (ret == 0) {
        LOGGER.error("GetCurrentDirectoryW failed with {}", ret);
        return null;
      }
      if (ret >= bufferLength) {
        LOGGER.error("Current working directory exceeds buffer size: {}", ret);
        return null;
      }
      return fromWideCString(ptr, ret);
    } finally {
      UnmanagedMemory.free(ptr);
    }
  }

  @Override
  public boolean changeWorkingDir(String path) {
    path = normalizeSlashes(path);
    var cPath = toWideCString(path);
    if (cPath == null) {
      LOGGER.error("Unable to allocate memory for {}", path);
      return false;
    }
    try {
      var res = SetCurrentDirectoryW(cPath);
      if (res == 0) {
        LOGGER.error("SetCurrentDirectoryW to {} failed with {}", path, res);
        return false;
      }
      return true;
    } catch (Throwable t) {
      LOGGER.error("Cannot change working directory to " + path + " on Windows", t);
      throw t;
    } finally {
      UnmanagedMemory.free(cPath);
    }
  }

  @Override
  public boolean exists(String dir, String file) {
    dir = normalizeSlashes(dir);
    file = normalizeSlashes(file);
    var full = dir + Platform.separatorChar() + file;
    var cPath = toWideCString(full);
    if (cPath == null) {
      LOGGER.error("Unable to allocate memory for {}", full);
      return false;
    }
    try {
      var res = PathFileExistsW(cPath);
      return res != 0;
    } catch (Throwable t) {
      LOGGER.error("Cannot check if {} exists on Windows", full, t);
      return false;
    } finally {
      UnmanagedMemory.free(cPath);
    }
  }

  private static CShortPointer toWideCString(String path) {
    var withTerminator = path.length() + 1;
    CShortPointer ptr = UnmanagedMemory.malloc(withTerminator * Character.BYTES);
    if (ptr == null) {
      return null;
    }
    for (int i = 0; i < path.length(); i++) {
      ptr.write(i, (short) path.charAt(i));
    }
    ptr.write(path.length(), (short) 0);
    return ptr;
  }

  private static String fromWideCString(CShortPointer ptr, int len) {
    var chars = new char[len];
    for (int i = 0; i < len; i++) {
      chars[i] = (char) ptr.read(i);
    }
    return new String(chars);
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
   * href="https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-getcurrentdirectoryw">Official
   * docs</a>
   */
  @CFunction
  static native int GetCurrentDirectoryW(int nBufferLength, CShortPointer lpBuffer);

  /**
   * <a
   * href="https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-setcurrentdirectoryw">Official
   * docs</a>
   */
  @CFunction
  static native int SetCurrentDirectoryW(CShortPointer lpPathName);

  /**
   * <a
   * href="https://learn.microsoft.com/en-us/windows/win32/api/shlwapi/nf-shlwapi-pathfileexistsw">Official
   * docs</a>
   */
  @CFunction
  static native int PathFileExistsW(CShortPointer pszPath);

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
