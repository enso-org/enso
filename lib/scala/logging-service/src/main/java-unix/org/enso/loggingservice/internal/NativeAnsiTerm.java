package org.enso.loggingservice.internal;

/**
 * Provides a stub for enabling VT console mode.
 * <p>
 * We assume that VT is supported by default on UNIX platforms, so this function is never actually
 * used. It is defined just to provide binary compatibility with the Windows counterpart, so that
 * the code that uses it only on Windows, compiles on all platforms.
 */
public class NativeAnsiTerm {

  /**
   * Enables VT emulation within the connected console.
   * <p>
   * The UNIX variant does nothing, as we assume that VT is supported out of the box.
   */
  public static void enableVT() {
  }
}
