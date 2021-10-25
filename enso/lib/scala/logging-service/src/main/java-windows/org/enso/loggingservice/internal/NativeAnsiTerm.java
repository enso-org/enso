package org.enso.loggingservice.internal;

import org.graalvm.nativeimage.UnmanagedMemory;
import org.graalvm.nativeimage.c.function.CFunction;
import org.graalvm.nativeimage.c.type.CIntPointer;
import org.graalvm.word.PointerBase;

/** Provides access to the native WinApi calls that enable VT emulation in a connected console. */
public class NativeAnsiTerm {

  /**
   * Returns a handle to a console connected to one of the standard streams.
   *
   * <p>Refer to: https://docs.microsoft.com/en-us/windows/console/getstdhandle
   *
   * @param nStdHandle constant representing one of the standard streams
   * @return pointer to the console handle or null if it could not be accessed
   */
  @CFunction
  private static native PointerBase GetStdHandle(int nStdHandle);

  /**
   * Returns current console mode.
   *
   * @param hConsoleHandle console handle from [[GetStdHandle]]
   * @param lpMode pointer to an integer that will be set to the current mode on success
   * @return non-zero integer on success
   * @see <a
   *     href="https://docs.microsoft.com/en-us/windows/console/setconsolemodehttps://docs.microsoft.com/en-us/windows/console/getconsolemode>GetConsoleMode</a>
   */
  @CFunction
  private static native int GetConsoleMode(PointerBase hConsoleHandle, CIntPointer lpMode);

  /**
   * Sets console mode.
   *
   * @param hConsoleHandle console handle from [[GetStdHandle]]
   * @param dwMode mode to set
   * @return non-zero integer on success
   * @see <a
   *     href="https://docs.microsoft.com/en-us/windows/console/setconsolemode">SetConsoleMode</a>
   */
  @CFunction
  private static native int SetConsoleMode(PointerBase hConsoleHandle, int dwMode);

  /**
   * Returns error code of last error.
   *
   * <p>Can be called if a function returns a zero exit code to get the error code.
   *
   * @see <a
   *     href="https://docs.microsoft.com/en-gb/windows/win32/api/errhandlingapi/nf-errhandlingapi-getlasterror">GetLastError</a>
   */
  @CFunction
  private static native int GetLastError();

  /**
   * Constant that can be used in [[GetStdHandle]] that refers to the standard error stream.
   *
   * @see <a href="https://docs.microsoft.com/en-us/windows/console/getstdhandle">GetStdHandle</a>
   */
  private static final int STD_ERROR_HANDLE = -12;

  /**
   * Constant that can be used as part of a console mode which indicates that the output stream
   * should handle VT escape codes.
   *
   * @see <a
   *     href="https://docs.microsoft.com/en-us/windows/console/setconsolemode">SetConsoleMode</a>
   * @see <a
   *     href="https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences">Console
   *     Virtual Terminal Sequences</a>
   */
  private static final int ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004;

  /**
   * Enables VT emulation within the connected console.
   *
   * <p>May throw an exception if it is not possible to do so. Can only be called from native-image
   * targets.
   */
  public static void enableVT() {
    CIntPointer modePtr = UnmanagedMemory.malloc(4);
    try {
      var handle = GetStdHandle(STD_ERROR_HANDLE);
      if (handle.isNull()) {
        throw new RuntimeException(
            "Failed to get console handle. "
                + "Perhaps the console is not connected. "
                + "Error code: "
                + GetLastError());
      }
      if (GetConsoleMode(handle, modePtr) == 0) {
        throw new RuntimeException(
            "Failed to get console mode. " + "Error code: " + GetLastError());
      }
      var alteredMode = modePtr.read() | ENABLE_VIRTUAL_TERMINAL_PROCESSING;
      if (SetConsoleMode(handle, alteredMode) == 0) {
        throw new RuntimeException(
            "Failed to set console mode. "
                + "Perhaps the console does not support VT codes. "
                + "Error code: "
                + GetLastError());
      }
    } finally {
      UnmanagedMemory.free(modePtr);
    }
  }
}
