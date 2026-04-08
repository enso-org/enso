package org.enso.os.environment.chdir;

import java.nio.ByteOrder;
import java.nio.CharBuffer;
import java.nio.charset.StandardCharsets;
import java.util.List;
import org.enso.common.Platform;
import org.graalvm.nativeimage.StackValue;
import org.graalvm.nativeimage.c.CContext;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.struct.CPointerTo;
import org.graalvm.nativeimage.c.struct.SizeOf;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.graalvm.word.PointerBase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@CContext(WindowsWorkingDirectory.Directives.class)
final class WindowsWorkingDirectory extends WorkingDirectory {
  static final WindowsWorkingDirectory INSTANCE = new WindowsWorkingDirectory();
  private static final Logger LOGGER = LoggerFactory.getLogger(WindowsWorkingDirectory.class);

  // Windows MAX_PATH is 260, but GetCurrentDirectoryW can return up to 32767 characters if the path is prefixed with \\?\.
  private static final int MAX_LENGTH = 32767;

  @Override
  public String currentWorkingDir() {
    var buffer = StackValue.get(MAX_LENGTH, WCharPointer.class);
    var length = GetCurrentDirectoryW(MAX_LENGTH, buffer);
    if (length == 0 || length == MAX_LENGTH) {
      return null;
    }
    return asCharBuffer(buffer, length).toString();
  }

  @Override
  public boolean changeWorkingDir(String path) {
    path = normalizeSlashes(path);
    var buffer = StackValue.get(MAX_LENGTH, WCharPointer.class);
    asCharBuffer(buffer, MAX_LENGTH).append(path).append('\0');

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
    }
  }

  @Override
  public boolean exists(String dir, String file) {
    dir = normalizeSlashes(dir);
    file = normalizeSlashes(file);

    var full = dir + Platform.separatorChar() + file;
    var buffer = StackValue.get(MAX_LENGTH, WCharPointer.class);
    asCharBuffer(buffer, MAX_LENGTH).append(full).append('\0');

    try {
      var res = PathFileExistsW(buffer);
      return res != 0;
    } catch (Throwable t) {
      LOGGER.error("Cannot check if file " + full + " exists on Windows", t);
      throw t;
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

  @CPointerTo(nameOfCType = "wchar_t")
  interface WCharPointer extends PointerBase {
  }

  private static CharBuffer asCharBuffer(WCharPointer wcString, int length) {
    /*
     * Wide characters encoded using UTF-16LE (for little-endian) are the native character
     * format on Windows, so we can simply wrap wide strings without any conversion.
     */
    return CTypeConversion.asByteBuffer(wcString, length * SizeOf.get(WCharPointer.class))
        .order(ByteOrder.LITTLE_ENDIAN).asCharBuffer();
  }

  /**
   * <a
   * href="https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-getcurrentdirectoryw">Official
   * docs</a>
   */
  @CFunction(transition = CFunction.Transition.NO_TRANSITION)
  static native int GetCurrentDirectoryW(int nBufferLength, WCharPointer lpBuffer);

  /**
   * <a
   * href="https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-setcurrentdirectoryw">Official
   * docs</a>
   */
  @CFunction
  static native int SetCurrentDirectoryW(WCharPointer lpPathName);

  /**
   * <a
   * href="https://learn.microsoft.com/en-us/windows/win32/api/shlwapi/nf-shlwapi-pathfileexistsw">Official
   * docs</a>
   */
  @CFunction
  static native int PathFileExistsW(WCharPointer pszPath);

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
